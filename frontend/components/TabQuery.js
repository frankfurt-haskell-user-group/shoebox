import React, { Component } from 'react';

function isArray(a) {
    return (!!a) && (a.constructor === Array);
};

function isObject(a) {
    return (!!a) && (a.constructor === Object);
};

function listGroupItem(i) {
  return (<li className="list-group-item">{i}</li>);
}

function listGroup(l) {
  return (
    <ul className="list-group">
    { l.map( (i) => {return <li className="list-group-item">{i}</li> }) }
    </ul>
    );
}

function showDetail(i) {

  if (isObject(i)) { 
      console.log("object: " + JSON.stringify(i)) ;
      var r = [] ; 
      for (var k in i) {
        r.push (
          <div>
            <h4>{k}</h4>
            {  showDetail(i[k]) }
          </div>
        );
      };
      return listGroup(r);
  } 

  if (isArray(i)) {
      console.log("array: " + JSON.stringify(i)) ;
      return (<div> { i.map( (n) => showDetail(n) ) }</div>)
  }

  console.log("non of both: " + JSON.stringify(i));
  return (<div>{i}</div>);
}

function showResult(json) {
  if (json != null && json.msg == "query-result") {
  return (
    <div>
      {showDetail(json.para)}
    </div>
    );
  } else {
    return (<div>no result</div>);
  }
}

class TabQuery extends React.Component {
  // render function 
  render() {
    return (
    <div>
    <div className="row">
      <div className="col-sm-12">
        <h3><small>Query</small></h3>
      </div>
    </div>
    <div className="row">
      <div className="form-group col-sm-8">
        <input style={{width: 500 + "px"}} type="text" className="form-control" id="query"></input>
      </div>
      <div className="col-sm-4">
        <button className="btn btn-primary btn-sm" onClick={ () => {
          var cmd = $('#query')[0].value;
          this.props.sbc.callShoebox("query", cmd);
        }}>
        Query DB
        </button>
      </div>
    </div>

    <div className="row">
      <div className="col-sm-12">
        <h3><small>Query Result</small></h3>
        {showResult(this.state.jsonResult)}
      </div>
    </div>

  </div>
      );  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { jsonResult : null };
    this.update = this.update.bind(this);
    }

    update(e, d) {
      this.setState({ jsonResult : d}); 
    }

    componentDidMount() {
      this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
      this.props.sbc.unsubscribe(this.update);
    } 

}

export default TabQuery; 