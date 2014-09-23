#!/bin/bash
if [ "$#" -ne 5 ]
then
    echo "usage : deploy.sh repo env imgname SHA 3 -- start three new repo/env instances of imgname:SHA"
    exit -1
fi
REPO=$1
ENV=$2
SLUG=${REPO}_${ENV}
IMG=$3
SHA=$4
NUM=$5
CFG=prod_${SHA}.cfg

# Get correct revision of prod.cfg
cd /srv/$REPO
git fetch
git checkout $SHA
cd ..
if [ ! -e $REPO/.rivetcrypt ]
then
    echo ".rivetcrypt does not exist at ${SHA}, aborting."
    exit -1
fi
openssl enc -aes-256-cbc -d -a -salt -in $REPO/.rivetcrypt -out ${CFG} -pass file:.rivetpass


echo "Logging in..."
docker login -e {{docker_email}} -u {{docker_username}} -p {{docker_password}}

echo "Getting currently running containers..."
OLDPORTS=( `docker ps | grep $SLUG | awk '{print $1}'` )
echo "pulling version $SHA"
docker pull $IMG:$SHA
echo "starting new containers"
for i in `seq 1 $NUM` ; do
    echo "inside loop $i"
    UNIQ=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 8 | head -n 1 | tr '[:upper:]' '[:lower:]')
    JOB=`docker run -d -w /srv -p 8000 -v /srv/data:/srv/data -v /srv/${CFG}:/srv/prod.cfg -v /var/run/redis/redis.sock:/var/run/redis/redis.sock -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 --name=${SLUG}_${UNIQ} ${IMG}:${SHA} | cut -c1-12`
    if [ -z "$JOB" ]
    then
	echo "could not create new container. aborting."
	exit -1
    fi
    echo "adding new container $JOB"
    PORT=`docker inspect $JOB | grep HostPort | cut -d '"' -f 4 | grep -v '^$'`
    if [ -z "$PORT" ]
    then
	echo "could not find PORT for container. aborting."
	exit -1
    fi
    etcdctl set "${REPO}/${ENV}/upstream/${JOB}" "127.0.0.1:$PORT"
done

# TODO(dbp 2014-09-23): We actually want to make sure what is in
# etcdctl does not have stale (ie, failed) containers. And we want
# to run confd even if there were no running containers.
echo "removing old containers"
for i in ${OLDPORTS[@]}
do
    echo "removing old container $i"
    etcdctl rm /${REPO}/${ENV}/upstream/$i
    sudo /usr/local/bin/confd -onetime
    sleep 1
    docker kill $i
done
