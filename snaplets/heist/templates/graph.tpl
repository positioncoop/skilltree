<apply template="base">
  <div class="section-tree">
    <svg class='tree' width=600 height=1000></svg>
  </div>

  <div class="section-tutorial">
    <div class="modeTray">
    </div>
    <p>
      <ifLoggedIn>
	Hi, <loggedInUser><userLogin /></loggedInUser>
	<a href="/auth/logout">Logout</a>
      </ifLoggedIn>
      <ifLoggedOut>
	<a href="/auth/login">Login</a>
      </ifLoggedOut>
    </p>
    <apply-content/>
  </div>

  <div class='courses'></div>
</apply>
