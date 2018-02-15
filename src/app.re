let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self =>
    <Layout>
      <div className="header" />
      <Content />
      <div className="footer" />
    </Layout>
};