<div class="tutorial-steps">
  <tutorialSteps>
    <div class="tutorial-step">
      <ifLoggedIn>
        <div class="edit-links">
          <a href="${stepEditPath}">Edit Video</a>
          <a class="delete" onclick="return confirm('Are you sure you want to delete this step? This action cannot be undone.')" href="${stepDeletePath}">Delete Video</a>
        </div>
      </ifLoggedIn>
      <div class="tutorial-step-inner">
        <div class="step-description">
          <stepContent/>
        </div>
        <div class="step-video">
          <stepVideo>
      <iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
          </stepVideo>
        </div>
      </div>
    </div>
  </tutorialSteps>
  <ifLoggedIn>
    <div class="edit-links">
      <a href="${tutorialStepNewPath}">Add a step!</a>
    </div>
    <div class="empty-step"></div>
  </ifLoggedIn>
 
</div>
