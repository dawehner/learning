use nom::{
    alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, line_ending, not_line_ending, space1},
    complete, do_parse, map, named, separated_list, tag,
};
use std::fmt;
use std::iter::{FromIterator, IntoIterator, Iterator};

#[derive(Debug)]
pub struct HostFile {
    pub vec: Vec<HostRow>,
}

impl HostFile {
    pub fn new(vec: Vec<HostRow>) -> HostFile {
        HostFile { vec: vec }
    }

    // pub fn iter(&self) -> std::slice::Iter<HostRow> {
    //     self.vec.iter()
    // }
}

impl IntoIterator for HostFile {
    type Item = HostRow;
    type IntoIter = ::std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl FromIterator<HostRow> for HostFile {
    fn from_iter<I: IntoIterator<Item = HostRow>>(iter: I) -> Self {
        HostFile::new(Vec::from_iter(iter))
    }
}

impl fmt::Display for HostFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for host_row in &self.vec {
            write!(f, "{}\n", host_row)?;
        }
        Ok(())
    }
}

impl PartialEq for HostFile {
    fn eq(&self, other: &Self) -> bool {
        self.vec == other.vec
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum HostRow {
    HostComment(String),
    HostPair(Ip, String),
    EmptyRow,
}

impl fmt::Display for HostRow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HostRow::EmptyRow => write!(f, "{}", ""),
            HostRow::HostComment(comment) => write!(f, "# {}", comment),
            HostRow::HostPair(ip, host) => write!(f, "{} {}", ip, host),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ip {
    Ipv4(Vec<String>),
    Ipv6(String),
}

impl fmt::Display for Ip {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ip::Ipv4(numbers) => write!(f, "{}", numbers.join(".")),
            Ip::Ipv6(string) => write!(f, "{}", string),
        }
    }
}

named!(pub parse_hosts<&str, HostFile>,
    map!(separated_list!(line_ending,
      alt!(parse_host_comment | parse_host_pair | parse_empty_line
    )), HostFile::new));

named!(parse_empty_line<&str, HostRow>,
  map!(tag!(""), |_| HostRow::EmptyRow)
);

named!(parse_host_comment<&str, HostRow>, do_parse!(
      tag!("#") >>
      comment: not_line_ending >>
      (HostRow::HostComment(comment.to_string()))
  )
);

named!(parse_ipv4<&str, Ip>, map!(separated_list!(tag("."), digit1), |vec| Ip::Ipv4(vec.iter().map(|&s| String::from(s)).collect())));

named!(parse_ipv6<&str, Ip>, map!(tag!("::1"), |ip| Ip::Ipv6(ip.to_string())));

named!(parse_hostname<&str, String>, alt!(do_parse!(
  hostname: alphanumeric1 >>
  complete!(tag!(".")) >>
  tld: complete!(alpha1) >>
  (format!("{}.{}", hostname, tld))
) | complete!(map!(alphanumeric1, String::from))
)
);

named!(parse_host_pair<&str, HostRow>, do_parse!(
    ip: alt!(parse_ipv6 | parse_ipv4) >>
    space1 >>
    host: parse_hostname >>
    (HostRow::HostPair(ip, host.to_string()))
));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ipv4() {
        assert_eq!(
            parse_ipv4("127.0.0.1"),
            Ok((
                "",
                Ip::Ipv4(vec![
                    "127".to_string(),
                    "0".to_string(),
                    "0".to_string(),
                    "1".to_string()
                ])
            ))
        );
        assert_eq!(
            parse_ipv4("192.168.1.2"),
            Ok((
                "",
                Ip::Ipv4(vec![
                    "192".to_string(),
                    "168".to_string(),
                    "1".to_string(),
                    "2".to_string()
                ])
            ))
        );
    }

    #[test]
    fn test_parse_ipv6() {
        assert_eq!(parse_ipv6("::1"), Ok(("", Ip::Ipv6("::1".to_string()))));
        //     assert_eq!(
        //         parse_ipv6("2001:0db8:0000:0000:0000:ff00:0042:8329"),
        //         Ok((
        //             "",
        //             Ip::Ipv6("2001:0db8:0000:0000:0000:ff00:0042:8329".to_string())
        //         ))
        //     );
    }

    #[test]
    fn test_parse_hostname() {
        assert_eq!(parse_hostname("heise.de"), Ok(("", "heise.de".to_string())));
        assert_eq!(
            parse_hostname("localhost"),
            Ok(("", "localhost".to_string()))
        );
        assert_eq!(
            parse_hostname("example.loc"),
            Ok(("", "example.loc".to_string()))
        );
        assert_eq!(
            parse_hostname("example2.loc"),
            Ok(("", "example2.loc".to_string()))
        );
    }

    #[test]
    fn test_parse_host_pair() {
        assert_eq!(
            parse_host_pair("127.0.0.1 localhost"),
            Ok((
                "",
                (HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "localhost".to_string()
                ))
            ))
        );
        assert_eq!(
            parse_host_pair("212.211.12.5 heise.de"),
            Ok((
                "",
                (HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "212".to_string(),
                        "211".to_string(),
                        "12".to_string(),
                        "5".to_string()
                    ]),
                    "heise.de".to_string()
                ))
            ))
        );
        assert_eq!(
            parse_host_pair("::1 localhost"),
            Ok((
                "",
                (HostRow::HostPair(Ip::Ipv6("::1".to_string()), "localhost".to_string()))
            ))
        );
    }

    #[test]
    fn test_parse_host_comment() {
        assert_eq!(
            parse_host_comment("#test"),
            Ok(("", HostRow::HostComment("test".to_string())))
        );
        assert_eq!(
            parse_host_comment("##test"),
            Ok(("", HostRow::HostComment("#test".to_string())))
        );
        assert_eq!(
            parse_host_comment("# test"),
            Ok(("", HostRow::HostComment(" test".to_string())))
        );
    }

    #[test]
    fn test_empty_line() {
        assert_eq!(parse_empty_line("#\n"), Ok(("#\n", HostRow::EmptyRow)));
        assert_eq!(parse_empty_line(""), Ok(("", HostRow::EmptyRow)));
        assert_eq!(parse_empty_line("\n"), Ok(("\n", HostRow::EmptyRow)));
    }

    #[test]
    fn test_parse_full_file() {
        assert_eq!(
            parse_hosts(
                "##
# Host Database
#
# localhost is used to configure the loopback interface
# when the system is booting.  Do not change this entry.
##
127.0.0.1 localhost
255.255.255.255	broadcasthost
::1 localhost

127.0.0.1 example.loc
127.0.0.1 d2.loc

#
#

# D4
127.0.0.1 example.loc

# Meh
#
127.0.0.1 other.loc

#

# Muh
# BEGIN section for OpenVPN Client SSL sites
127.94.0.1	client.openvpn.net
# END section for OpenVPN Client SSL sites
"
            )
            .unwrap()
            .1,
            HostFile::new(vec![
                HostRow::HostComment("#".to_string()),
                HostRow::HostComment(" Host Database".to_string()),
                HostRow::HostComment("".to_string()),
                HostRow::HostComment(
                    " localhost is used to configure the loopback interface".to_string()
                ),
                HostRow::HostComment(
                    " when the system is booting.  Do not change this entry.".to_string()
                ),
                HostRow::HostComment("#".to_string()),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "localhost".to_string()
                ),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "255".to_string(),
                        "255".to_string(),
                        "255".to_string(),
                        "255".to_string()
                    ]),
                    "broadcasthost".to_string()
                ),
                HostRow::HostPair(Ip::Ipv6("::1".to_string()), "localhost".to_string()),
                HostRow::EmptyRow,
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "example.loc".to_string()
                ),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "d2.loc".to_string()
                ),
                HostRow::EmptyRow,
                HostRow::HostComment("".to_string()),
                HostRow::HostComment("".to_string()),
                HostRow::EmptyRow,
                HostRow::HostComment(" D4".to_string()),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "example.loc".to_string()
                ),
                HostRow::EmptyRow,
                HostRow::HostComment(" Meh".to_string()),
                HostRow::HostComment("".to_string()),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "0".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "other.loc".to_string()
                ),
                HostRow::EmptyRow,
                HostRow::HostComment("".to_string()),
                HostRow::EmptyRow,
                HostRow::HostComment(" Muh".to_string()),
                HostRow::HostComment(" BEGIN section for OpenVPN Client SSL sites".to_string()),
                HostRow::HostPair(
                    Ip::Ipv4(vec![
                        "127".to_string(),
                        "94".to_string(),
                        "0".to_string(),
                        "1".to_string()
                    ]),
                    "client.openvpn".to_string()
                )
            ])
        );
    }

    #[test]
    fn test_display_ip() {
        assert_eq!(
            format!(
                "{}",
                Ip::Ipv4(vec![
                    "127".to_string(),
                    "0".to_string(),
                    "0".to_string(),
                    "1".to_string()
                ])
            ),
            "127.0.0.1"
        );
        assert_eq!(format!("{}", Ip::Ipv6("::1".to_string())), "::1");
    }

    #[test]
    fn test_display_hostrow() {
        assert_eq!(format!("{}", HostRow::EmptyRow), "");
        assert_eq!(
            format!("{}", HostRow::HostComment("test".to_string())),
            "# test"
        );
        assert_eq!(
            format!(
                "{}",
                HostRow::HostPair(Ip::Ipv6("::1".to_string()), "localhost".to_string())
            ),
            "::1 localhost"
        );
    }

    #[test]
    fn test_display_hostfile() {
        assert_eq!(
            format!(
                "{}",
                HostFile::new(vec![
                    HostRow::EmptyRow,
                    HostRow::HostComment("test".to_string()),
                    HostRow::HostPair(Ip::Ipv6("::1".to_string()), "localhost".to_string())
                ])
            ),
            "
# test
::1 localhost
"
        )
    }
}
