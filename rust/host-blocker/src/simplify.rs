use crate::hostfile::*;
use std::iter::Iterator;

pub fn simplify(hostfile: HostFile) -> HostFile {
    let mut hostfile2 = simplify_more_than_three_emptyrow(hostfile);
    hostfile2 = simplify_remove_trailing_space_comment(hostfile2);
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

fn simplify_remove_trailing_space_comment(hostfile: HostFile) -> HostFile {
    HostFile::new(
        hostfile
            .vec
            .into_iter()
            .map(|row| match row {
                HostRow::HostComment(comment) => {
                    HostRow::HostComment(comment.trim_start().to_string())
                }
                _ => row,
            })
            .collect(),
    )
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

    #[test]
    fn test_remove_trailing_space_comment() {
        let hostfile = HostFile::new(vec![
            HostRow::HostComment("test".to_string()),
            HostRow::HostComment(" test".to_string()),
        ]);
        let expected = HostFile::new(vec![
            HostRow::HostComment("test".to_string()),
            HostRow::HostComment("test".to_string()),
        ]);

        assert_eq!(simplify(hostfile), expected);
    }

}
