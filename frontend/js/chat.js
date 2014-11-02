window.addEventListener('load', function() {
  var sendJSON = function(url, body) {
    return new Promise(function(resolve, reject) {
      var req = new XMLHttpRequest();
      req.open('POST', url, true);
      req.onreadystatechange = function() {
        if (req.readyState == 4) {
          if (req.status == 200) {
            resolve(JSON.parse(req.responseText));
          }
          else {
            reject(req.statusText);
          }
        }
      }
      req.send(JSON.stringify(body));
    });
  };

  var Chat = React.createClass({
    getInitialState: function() {
      return {
        login: ''
      };
    },

    onLogin: function(login) {
      this.setState({
        login: login
      });
    },

    render: function () {
      return (
        <div className="b-chat">
          {this.state.login ? <div className="b-chat__messages"></div> : <AuthForm onLogin={this.onLogin.bind(this)} />}
        </div>
      )
    }
  });

  var AuthForm = React.createClass({
    getInitialState: function() {
      return {
        error: ''
      };
    },
    getDefaultProps: function() {
      return {
        onLogin: function(login) {
          console.log('success auth with login %s', login);
        }
      };
    },
    onLoginClick: function() {
      var login = this.refs.input.getDOMNode().value;
      sendJSON('/api/auth', {
        'login': login
      }).then(function(resp) {
        if (resp.status == 'success') {
          this.props.onLogin(login);
        }
        else {
          function errMsg(status) {
            switch (status) {
              case 'alreadyExists':
                return 'User with login ' + login + ' is already exists';
            }
          }
          this.setState({
            error: errMsg(resp.status)
          });
        }
      }.bind(this), function(err) {
        console.log('err', err);
      });
    },

    render: function() {
      return (
        <div className="b-auth-form">
          <label>Login: <input className="b-auth-form__login"  ref="input" /></label>
          <button className="b-auth-form" onClick={this.onLoginClick}>Login</button>
          {this.state.error ? <div className="b-auth-form__error">{this.state.error}</div>: ""}
        </div>
      );
    }
  });

  React.render(
    <Chat />,
    document.body
  );
});
