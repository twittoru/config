# require_gem でも正しく動作するように
require 'rubygems'

# tab で補完
require 'irb/completion'

# 3.14.what? 4 で 3.14.ceil == 4 => ["ceil"]
# という風に結果からメソッドを検索
require 'what_methods'

# p よりも見やすく表示
require 'pp'

# オートインデントを使う
IRB.conf[:AUTO_INDENT]=true
# ヒストリを 200 保存
IRB.conf[:SAVE_HISTORY]=200
