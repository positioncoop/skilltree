<apply template="base">
  <svg class='grid' width=600 height=1000>
  </svg>

  <div style='float:right'>
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
</apply>
