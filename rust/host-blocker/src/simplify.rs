use crate::hostfile::*;

pub fn simplify(hostfile: HostFile) -> HostFile {
    let hostfile2 = simplify_more_than_three_emptyrow(hostfile);
    hostfile2
}

fn simplify_more_than_three_emptyrow(hostfile: HostFile) -> HostFile {
    let mut rows = hostfile.vec;
    let mut counter = 0usize;
    rows.retain(|row| {
        if *row == HostRow::EmptyRow {
            counter += 1;
            counter <= 2
        } else {
            counter = 0;
            true
        }
    });

    HostFile::new(rows)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiple_emptyrow() {
        let hostfile2 = HostFile::new(vec![HostRow::EmptyRow, HostRow::EmptyRow]);
        let hostfile3 = HostFile::new(vec![
            HostRow::EmptyRow,
            HostRow::EmptyRow,
            HostRow::EmptyRow,
        ]);
        let hostfile4 = HostFile::new(vec![
            HostRow::EmptyRow,
            HostRow::EmptyRow,
            HostRow::EmptyRow,
            HostRow::EmptyRow,
        ]);

        assert_eq!(simplify(hostfile3), hostfile2);
        assert_eq!(simplify(hostfile4), hostfile2);
    }

}
