module Quotients = {
  [@react.component]
  let make = (~quoCount, ~quotients) =>
    <form
      onSubmit={e => {
        React.Event.Form.preventDefault(e);
        let elts = React.Event.Form.target(e)##elements->Belt.List.fromArray;
        /* We need to ensure we don't include submit button */
        switch (Belt.List.take(elts, quoCount)) {
        | Some(qs) =>
          let values = Belt.List.map(qs, elt => elt##value);
          Js.log(values);
        | None => Js.log("nothing")
        };
      }}>
      {Belt.List.mapWithIndex(quotients, (i, q_i) =>
         <div>
           <label>
             {React.string("q_" ++ Belt.Int.toString(i))}
             <input value=q_i />
           </label>
         </div>
       )
       ->Belt.List.toArray
       ->React.array}
      <input type_="submit" />
    </form>;
};

module PolyDiv = {
  [@react.component]
  let make = () => {
    let (quoCount, setQuoCount) = React.useState(_ => 1);
    let (quotients, _setQuotients) = React.useState(_ => ["a", "b", "c"]);
    <>
      <label>
        {React.string("number of quotients: ")}
        <input
          type_="number"
          value={Belt.Int.toString(quoCount)}
          onChange={event => {
            let value = React.Event.Form.target(event)##value;
            setQuoCount(_ => value);
          }}
        />
      </label>
      <Quotients quoCount quotients />
    </>;
  };
};

module App = {
  // This sample forces an import of Belt.*, so that CI builds can ensure that
  // Melange has been installed correctly for JS bundlers to be able to find it.
  [@react.component]
  let make = () => <PolyDiv />;
};

switch (ReactDOM.querySelector("#root")) {
| Some(element) =>
  let root = ReactDOM.Client.createRoot(element);
  ReactDOM.Client.render(root, <App />);
| None =>
  Js.Console.error("Failed to start React: couldn't find the #root element")
};
