<apply template="graph">
  <div class="section-tutorial-inner edit">
  <dfForm class="form-tutorial">
    <div class="header">
      <img class="icon" src="${tutorialIconPath}" />
      <dfInputText class="title" ref="title" size="40" placeholder="Title"/>
      <dfChildErrorList ref="title" />
      <a href="${tutorialShowPath}">Preview</a>
    </div>
    <br />
    <dfInputFile ref="iconPath" onchange="$(this).parents('form').submit()"/>
    <dfChildErrorList ref="iconPath" />
    <br />
    <dfInputSelect ref="publish" />
    <dfChildErrorList ref="publish" />
    <br />
    <dfInputSubmit value="Save" class="btn btn-lg btn-primary btn-block" />
  </dfForm>
  <a onclick="return confirm('Are you sure you want to delete this?')" href="${tutorialDeletePath}">Delete this tutorial!</a>
  <hr/>

  <a href="${tutorialStepNewPath}">Add a step!</a>

  <apply template="tutorial-steps"></apply>

  </div>
</apply>
