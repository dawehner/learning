import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/otp/basic_supervisor.{Spec, WorkerSpec, OneForAll, Permanent}
import toplist_sites

pub external type Node;

pub type StartType {
  Normal
  Takeover(Node)
  Failover(Node)
};

pub fn start(_: StartType, _: Dynamic) {
  basic_supervisor.start_link(Spec(
    strategy: OneForAll,
    intensity: 1,
    period: 5,
    children: [
      basic_supervisor.make_child_opaque(WorkerSpec(
        id: "toplist_sites",
        start: toplist_sites.start_link,
        restart: Permanent,
        shutdown: 500,
      )),
    ]
  ))
}

pub fn stop(_: Dynamic) {
  atom.create_from_string("ok")
}
