shell:
	nix-shell -p python3 python3.pkgs.mistune python3.pkgs.scrapy

run:
	nix-shell -p python3 python3.pkgs.mistune python3.pkgs.scrapy --command 'scrapy runspider awesome-spider.py --loglevel=INFO --output=result.json'

