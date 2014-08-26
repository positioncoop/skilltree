<apply template="base">
 <div class="container">
  <div class="section-logo">
    <img class="logo" src="${siteLogoPath}" />
  </div>
  <div class="section-tree">
    <svg class='tree'></svg>
  </div>

  <div class="section-tutorial">
    <apply-content/>
  </div>

  <div class='courses'></div>

  <div id="javascript-helpers" style="display:none;">
    <!-- this is hacky, but currently necessary - exposes variables such as default icons to javascript -->
    <img id="tutorialDefaultIconPath" src="${tutorialDefaultIconPath}" />
  </div>

 </div>



  <div class="modeTray">
    <ifLoggedIn>
        Hi, <loggedInUser><userLogin /></loggedInUser>
        <a href="/auth/logout">Logout</a>
    </ifLoggedIn>
    <ifLoggedOut>
        <a href="/auth/login">Login</a>
    </ifLoggedOut>
  </div>


</apply>
