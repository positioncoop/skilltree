<apply template="graph">
  <div class="section-tutorial-inner show">
    <div class="header">
      <img class="icon" src="${tutorialIconPath}" />
      <div class="title"><tutorialTitle /></div>
      <ifLoggedIn><a href="${tutorialEditPath}">Edit</a></ifLoggedIn>
    </div>
	<div class="tutorial-steps">
    <tutorialSteps>
      <div class="tutorial-step">
      	<div class="step-content">
          <stepContent/>
		</div>
		<div class="step-video">
          <stepVideo>
            <iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
          </stepVideo>
		</div>
      </div>
    </tutorialSteps>
    </div>
  </div>
</apply>
