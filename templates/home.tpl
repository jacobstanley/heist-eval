<apply template="main">
  <h1>Home Page</h1>
  <p>Welcome to XYZ Inc</p>
  <p>We have <foo>BAR</foo>!</p>

  <div each="foos">
    <h2><quux /></h2>
    <p><snaffle /></p>
  </div>

  <div each="bars">
    <h2><value /></h2>
    <p><value /></p>
  </div>

  <div>
    <p  class=" [ foo bar ' ] ">No space between</p><p>These two</p>
  </div>
  <div>
    <p class="foo bar " >Joined with</p><!-- --><p>a comment</p>
  </div>
</apply>
