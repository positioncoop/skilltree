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

  <a href="${tutorialStepNewPath}">Add a step!</a>

  <tutorialSteps>
    <div>
      <stepContent/>
      <a href="${stepEditPath}">Edit</a>
    </div>
    <stepVideo>
      <iframe src="${url}" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
    </stepVideo>
  </tutorialSteps>

</apply>
