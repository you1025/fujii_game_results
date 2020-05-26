require "nokogiri"
require "open-uri"
require "csv"
require "pp"

urls = [
  "http://kenyu1234.php.xdomain.jp/persony.php?name=594&je=2016",
  "http://kenyu1234.php.xdomain.jp/persony.php?name=594&je=2017",
  "http://kenyu1234.php.xdomain.jp/persony.php?name=594&je=2018",
  "http://kenyu1234.php.xdomain.jp/persony.php?name=594&je=2019"
]


def extract_from_html(html)
  doc = Nokogiri::HTML.parse(html)

  path = '//table[@id="matchTable1"]/tbody/*'

  data = []
  doc.xpath(path).each do |tr|
    line = []
    line << tr.xpath(".//td[2]").inner_text
    line << tr.xpath(".//td[3]").inner_text
    line << tr.xpath(".//td[4]").inner_text

    data << line
  end

  data
end

def load_from_urls(urls)
  urls.map do |url|
    html = URI.open(url) do |f|
      puts "scraping from #{url}"
      f.read
    end

    extract_from_html(html)
  end
end

# データの取得
fujii_results = load_from_urls(urls)

# ファイルへの書き出し
CSV.open("./data/fujii.csv", "wb") do |csv|

  # ヘッダの付加
  csv << ["date", "first_or_second", "result"]

  fujii_results.each do |data|
    data.each do |row|
      csv << row
    end
  end
end
