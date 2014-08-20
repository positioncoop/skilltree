<apply template="graph">
  <div class="section-tutorial-inner show">
    <div class="header">
      <img class="icon" src="${tutorialIconPath}" />
      <div class="title"><tutorialTitle /></div>
      <ifLoggedIn><a href="${tutorialEditPath}">Edit</a></ifLoggedIn>
    </div>
  	<apply template="tutorial-steps"></apply>
  </div>
</apply>
