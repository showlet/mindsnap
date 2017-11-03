<div class="navbar">
    <span>showlet's blog</span>
    <ifLoggedOut>
        <a href="/login" class="nav-login nav-elem">Login</a>
    </ifLoggedOut>
    <a href="/new_user" class="nav-newuser nav-elem">Sign up</a>
    <ifLoggedIn>
        <span>logged in as <loggedInUser /> </span>
    </ifLoggedIn>
</div>