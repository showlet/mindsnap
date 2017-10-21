<apply template="base">
  <h2>Mindsnap</h2>
  <p>A team task management platform</p>
  <p>This website is in construction</p>
  <a href="/login">log in</a>

  <ifLoggedIn>
  <hr/>
    <p>Hi <loggedInUser/></p>
    <p>You could always <a href="/logout">logout</a></p>
  </ifLoggedIn>

      <ignore>
      this is cool
        <ifLoggedOut>
        <apply template="_login"/>
      </ifLoggedOut>
    </ignore>
</apply>
