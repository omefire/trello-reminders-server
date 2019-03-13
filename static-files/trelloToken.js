(function() {
    let tokenHash = window.location.hash;
    const token = tokenHash.replace('#token=', '');

    const urlParams = new URLSearchParams(window.location.search);
    const trelloid = urlParams.get('trelloid');

    // 1- Retrieve the token & the trello id from the URL (done)
    // 2- Let the user know we are processing their request and show them a loading spinner (done)
    // 3- Send the token and trello id to the web service
    // 4- Process the response from the web service: (done)
    //   4.a- If it was successful, tell the user it's successful and ask them to go back to their Trello window & refresh (done)
    //   4.b- If it errored out, tell the user an error happened and ask them to contact info@trelloreminders.com (done)

    $('#loading').show();
    let data = {
      trelloID: trelloid,
      trelloToken: token
    };
    fetch('http://localhost:8081/setTrelloToken', {
      method: 'POST',
      body: JSON.stringify(data),
      headers: new Headers({
        "Content-Type": "application/json"
      })
    }).then(function(response) {
      return response.json();
    }).then(function(data) {
      $('#loading').hide();
      $('#success').show();
      console.log("Success");
    }).catch(err => {
        $('#loading').hide();
        $('#error').append(JSON.stringify(err));
        $('#error').show();
      console.log('Failure: ' + JSON.stringify(err));
    });

})();
