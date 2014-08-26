<apply template="graph">
  <div class="section-tutorial-inner edit">
  <dfForm class="form-tutorial">
    <div class="header">
      <div class="edit-links">
        <a href="${tutorialShowPath}">Preview</a>
      </div>
      <div class="header-inner">
        <div class="icon">
          <img src="${tutorialIconPath}" />
        </div>
        <div class="title">
          <dfInputTextArea class="title tutorial-title" ref="title" placeholder="Title"/>
          <dfChildErrorList ref="title" class="error" />
        </div>
      </div>
    </div>
    <br />
    <dfInputFile ref="iconPath" onchange="$(this).parents('form').submit()"/>
    <dfChildErrorList ref="iconPath" class="error" />
    <br />
    <dfInputSelect ref="publish" />
    <dfChildErrorList ref="publish" class="error" />
    <br />
    <dfInputSubmit value="Save" class="btn btn-lg btn-primary btn-block" />
  </dfForm>
  <a onclick="return confirm('Are you sure you want to delete this?')" href="${tutorialDeletePath}">Delete this tutorial!</a>
  <hr/>

  <apply template="tutorial-steps"></apply>


  </div>
</apply>
