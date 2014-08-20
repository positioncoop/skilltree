<div class="tutorial-steps">
  <tutorialSteps>
    <div class="tutorial-step">
      <stepContent/>
      <ifLoggedIn>
      <a href="${stepEditPath}">Edit</a>
      <a onclick="return confirm('Are you sure you want to delete this?')" href="${stepDeletePath}">Delete</a>
      <hr />
      </ifLoggedIn>
      <stepVideo>
	<iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
      </stepVideo>
    </div>
  </tutorialSteps>
</div>
