window.addEventListener('load', function() {
  var HOST = window.location.host;
  var ajax = function(url, method, body) {
    return new Promise(function(resolve, reject) {
      var req = new XMLHttpRequest();
      req.open(method, url, true);
      req.onreadystatechange = function() {
        if (req.readyState == 4) {
          if (req.status == 200) {
            resolve(req.responseText);
          }
          else {
            reject(req.statusText);
          }
        }
      }
      req.send(body);
    });
  }

  var sendJSON = function(url, body) {
    return ajax(url, 'POST', JSON.stringify(body)).then(JSON.parse);
  };

  var getJSON = function(url) {
    return ajax(url, 'GET', null).then(JSON.parse);
  };

  var Chat = React.createClass({
    getInitialState: function() {
      return {
        login: ''
      };
    },

    getAuthStatus: function() {
      getJSON('/api/status').then(function(resp) {
        if (resp.status == 'authorized') {
          this.setState({
            login: resp.login
          });
          this.getUsersList();
        }
      }.bind(this));
    },

    getUsersList: function() {
      getJSON('/api/users').then(function(resp) {
        console.log(resp);
      });
    },

    componentWillMount: function() {
      this.getAuthStatus();
    },

    onLogin: function(login) {
      this.setState({
        login: login
      });
      this.getUsersList();
    },

    onLogout: function() {
      this.setState({
        login: ''
      });
    },

    render: function () {
      return (
        <div className="b-chat">
          {(this.state.login
            ? <MessagesBox login={this.state.login} onLogout={this.onLogout} />
            : <AuthForm onLogin={this.onLogin} />
          )}
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
          <button className="b-auth-form__send" onClick={this.onLoginClick}>Login</button>
          {this.state.error ? <div className="b-auth-form__error">{this.state.error}</div>: ""}
        </div>
      );
    }
  });

  var MessagesBox = React.createClass({
    getInitialState: function() {
      return {
        conn: null,
        events: []
      }
    },
    onLogoutClick: function() {
      getJSON('/api/logout');
      this.state.conn.close();
      this.props.onLogout();
    },
    onSendClick: function() {
      if (this.state.conn) {
        var msgInput = this.refs.messageInput.getDOMNode();
        var msg = msgInput.value;
        if (msg.trim()) {
          this.state.conn.send(JSON.stringify({message: msg}));
          msgInput.value = '';
        }
      }
    },
    getDefaultProps: function() {
      return {
        onLogout: function() {
          console.log('logout');
        }
      }
    },
    onMessage: function(msg) {
      this.setState({
        events: this.state.events.concat([JSON.parse(msg.data)])
      });
    },
    componentWillMount: function() {
      var socket = new WebSocket('ws://' + HOST + '/chat/' + this.props.login);
      socket.onerror = function(err) {
        console.log('err', err);
      };
      socket.onmessage = this.onMessage.bind(this);
      this.setState({
        conn: socket
      });
    },
    render: function() {
      var chatEvents = this.state.events.map(function(ev) {
        switch  (ev.event) {
          case 'enter':
            return <div className="b-messages-box__enter-event">{ev.user} join to the chat</div>;
          case 'leave':
            return <div className="b-messages-box__leave-event">{ev.user} leave to the chat</div>;
          case 'message':
            return <div className="b-messages-box__message-event">{ev.user}: {ev.message}</div>;
        }
      });
      return (
        <div className="b-messages-box">
          <h2 className="b-messages-box__header">Your are logined as {this.props.login}</h2>
          {chatEvents}
          <input className="b-messages-box__input" ref="messageInput" value={this.state.message} />
          <button className="b-messages-box__send" onClick={this.onSendClick}>Send</button>
          <button className="b-messages-box__logout" onClick={this.onLogoutClick}>Logout</button>
        </div>
      );
    }
  });

  React.render(
    <Chat />,
    document.body
  );
});
