use anyhow::{Result, anyhow};
use jiff::Zoned;
use jiff::civil::Date;
use scraper::{ElementRef, Html, Selector};
use std::cmp::Ordering;
use std::time::Duration;

#[derive(Debug)]
struct BookInfo {
    day: Date,
    name: String,
}

fn main() -> Result<()> {
    let mut args = std::env::args_os();
    args.next();

    let day = match args.next() {
        Some(day) => Date::strptime(b"%Y-%m-%d", day.as_encoded_bytes())?,
        None => Zoned::now().date(),
    };

    const START_PAGE: u32 = 1;

    for n in START_PAGE.. {
        if n != START_PAGE {
            const TIME: Duration = Duration::from_millis(500);
            std::thread::sleep(TIME);
        }

        let books = parse(&fetch(n)?)?;
        for b in books {
            match b.day.cmp(&day) {
                Ordering::Less => return Ok(()),
                Ordering::Equal => {
                    println!("{}\t{}\n", b.day.strftime("%Y-%m-%d"), b.name);
                }
                Ordering::Greater => (),
            }
        }
    }

    Ok(())
}

fn parse(page: &str) -> Result<Vec<BookInfo>> {
    let html = Html::parse_document(page);
    let sel = Selector::parse("#comid0 tr:nth-child(2n+1)").map_err(|e| anyhow!(e.to_string()))?;

    let mut trs = html.select(&sel);
    trs.next();
    trs.next_back();

    #[derive(Debug, Default)]
    struct PartialInfo {
        day: Option<Date>,
        name: Option<String>,
    }

    impl PartialInfo {
        fn set_day(&mut self, day: Date) {
            self.day = Some(day);
        }

        fn set_name(&mut self, name: String) {
            self.name = Some(name);
        }

        fn into_book_info(self) -> Option<BookInfo> {
            match (self.day, self.name) {
                (Some(day), Some(name)) => Some(BookInfo { day, name }),
                _ => None,
            }
        }
    }

    let mut books = Vec::new();

    for tr in trs {
        let mut info = PartialInfo::default();

        for (i, td) in tr.child_elements().enumerate() {
            match i {
                1 => {
                    let text = text(&td);
                    let Some(pos) = text.find('(') else {
                        break;
                    };
                    let Ok(day) = Date::strptime("%Y/%m/%d", &text[..pos]) else {
                        break;
                    };
                    info.set_day(day)
                }
                4 => info.set_name(text(&td)),
                _ => (),
            }
        }

        if let Some(b) = info.into_book_info() {
            books.push(b);
        }
    }

    Ok(books)
}

fn text(e: &ElementRef<'_>) -> String {
    let mut text = String::new();
    for t in e.text() {
        text.push_str(t.trim());
    }
    text
}

fn fetch(n: u32) -> Result<String> {
    let url = format!("http://wawabook.com.tw/comic/0101.php?pagea={n}");
    Ok(reqwest::blocking::get(url)?.text()?)
}
