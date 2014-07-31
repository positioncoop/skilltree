<apply template="base">
  <dfForm class="form-tutorial">
    <dfInputText ref="title" size="40" placeholder="Title"/>
    <dfChildErrorList ref="title" />
    <br />
    <dfInputFile ref="iconPath" onchange="$(this).parents('form').submit()"/>
    <dfChildErrorList ref="iconPath" />
    <br />
    <dfInputSubmit value="Enter" class="btn btn-lg btn-primary btn-block" />
  </dfForm>
  <a onclick="return confirm('Are you sure you want to delete this?')" href="${tutorialDeletePath}">Delete this tutorial!</a>
  <hr/>

  <a href="${tutorialStepNewPath}">Add a step!</a>

  <tutorialSteps>
    <div class="tutorial-step">
      <stepContent/>
      <a href="${stepEditPath}">Edit</a>
      <a onclick="return confirm('Are you sure you want to delete this?')" href="${stepDeletePath}">Delete</a>
      <hr/>
      <stepVideo>
	<iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
      </stepVideo>
    </div>
  </tutorialSteps>

</apply>
