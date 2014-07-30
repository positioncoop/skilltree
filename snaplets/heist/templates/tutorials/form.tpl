<apply template="base">
  <dfForm class="form-tutorial">
    <dfInputText ref="title" size="40" placeholder="Title"/>
    <dfChildErrorList ref="title" />
    <br />
    <dfInputFile ref="iconPath" />
    <dfChildErrorList ref="iconPath" />
    <br />
    <dfInputSubmit value="Enter" class="btn btn-lg btn-primary btn-block" />
  </dfForm>

  <hr/>

  <a href="/tutorials/${tutorialId}/steps/new">New Step</a>


</apply>
