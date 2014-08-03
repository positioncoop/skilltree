<apply template="graph">
  <dfForm class="form-step">
    <dfInputTextArea ref="content" placeholder="Please describe the step"/>
    <dfChildErrorList ref="content" />
    <br />
    <dfInputText ref="ordinal" />
    <dfChildErrorList ref="ordinal" />
    <br />
    <dfInputText ref="video-code" />
    <dfChildErrorList ref="video-code" />
    <br />
    <dfInputSelect ref="video-provider" />
    <dfChildErrorList ref="video-provider" />
    <br />

    <dfInputSubmit value="Enter" class="btn btn-lg btn-primary btn-block" />
  </dfForm>
</apply>
