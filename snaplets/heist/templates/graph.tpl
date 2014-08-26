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

    <div class="modeTray">
      <ifLoggedIn>
        <div class="modeTray-inner-logged-in">
          Hi, <loggedInUser><userLogin /></loggedInUser>
          <a href="/auth/logout">Logout</a>
        </div>
      </ifLoggedIn>
      <ifLoggedOut>
        <div class="modeTray-inner-logged-out">
          <a href="/auth/login">Login</a>
        </div>
      </ifLoggedOut>
    </div>

  <div class='courses'></div>
 </div>
</apply>
