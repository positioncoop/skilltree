<apply template="graph">
  <div style="border:1px #efefef solid; padding: 10px; margin: 10px; width: 580px;">
    <div class="header">
      <img class="icon" src="${tutorialIconPath}" />
      <div class="title"><tutorialTitle /></div>
      <ifLoggedIn><a href="${tutorialEditPath}">Edit</a></ifLoggedIn>
    </div>
    <tutorialSteps>
      <div class="tutorial-step">
        <stepContent/>
        <hr/>
        <stepVideo>
          <iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
        </stepVideo>
      </div>
    </tutorialSteps>
  </div>
</apply>
