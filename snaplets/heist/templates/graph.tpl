<apply template="base">
  <svg class='grid' width=600 height=1000>
  </svg>

  <div style='float:right'>
    <ifLoggedIn>
      <h1>Hi,
	<loggedInUser>
	  <userLogin />
	</loggedInUser>
      </h1>
      <a href="/auth/logout">Logout</a>
    </ifLoggedIn>
    <ifLoggedOut>
      <a href="/auth/login">Login</a>
    </ifLoggedOut>
    <apply-content/>
  </div>
</apply>
