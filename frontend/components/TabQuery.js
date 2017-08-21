import React, { Component } from 'react';

function showLog(log) {
  return log.map( (d) => 
    <div>{JSON.stringify(d)}<br/></div>
    )
}

class TabQuery extends React.Component {
  // render function 
  render() {
  	return (
		<div>
       <h3><small>Query Panel</small></h3>
       <p/>
       <h4>Event Log:</h4>
        { showLog(this.state.msgLog) }
    </div>
  	);
  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { msgLog : [] };
    this.update = this.update.bind(this);
  	}

    update() {
      this.setState({msgLog : this.props.sbc.getLog()}); 
    }

    componentDidMount() {
      this.update(); 
      this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
      this.props.sbc.unsubscribe(this.update);
    }
}

export default TabQuery; 