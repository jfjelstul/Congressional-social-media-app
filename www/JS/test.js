/*var slider = document.getElementById("slider");
output.innerHTML = slider.value; // Display the default slider value

// Update the current slider value (each time you drag the slider handle)
slider.oninput = function() {
  output.innerHTML = this.value;
}*/

/*import React from "react";
import ReactDOM from "react-dom";
import Button from "material-ui";
import Button from "@material-ui/core/Button";*/


/*function App() {
  return (
    <Button variant="contained" color="primary">
      Hello World
    </Button>
    <p>React text.</p>
  );
}

ReactDOM.render(<App/>, document.querySelector("#app"));*/


'use strict';

const e = React.createElement;

class LikeButton extends React.Component {
  /*constructor(props) {
    super(props);
    this.state = { liked: false };
  }*/

  render() {
    /*?if (this.state.liked) {
      return 'You liked this.';
    }

    return e(
      'button',
      { onClick: () => this.setState({ liked: true }) },
      'Like'*/
      <p>Text.</p>
    );
  }
}

/*const domContainer = document.querySelector('#like_button_container');
ReactDOM.render(e(LikeButton), domContainer);*/
