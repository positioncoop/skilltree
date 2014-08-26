<apply template="graph">
  <div class="section-tutorial-inner show">
    <div class="header">
      <ifLoggedIn>
        <div class="edit-links">
          <a href="${tutorialEditPath}">Edit</a>
        </div>
      </ifLoggedIn>
      <div class="header-inner">
        <div class="icon">
          <img src="${tutorialIconPath}" />
        </div>
        <div class="title">
          <tutorialTitle />
        </div>
      </div>
    </div>
  	<apply template="tutorial-steps"></apply>
  </div>
</apply>
