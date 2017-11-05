<apply template="base">
  <apply template="_nav" />
  <apply template="_header" />
  <apply template="_posts" />

      <ignore>
        <ifLoggedOut>
        <apply template="_login"/>
      </ifLoggedOut>
      <ifLoggedIn>
        <p>Hi <loggedInUser/></p>
        <p>You could always <a href="/logout">logout</a></p>
      </ifLoggedIn>
    </ignore>
</apply>
