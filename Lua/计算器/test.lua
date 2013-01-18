require "qtcore"
require "qtgui"
require "scan"

local app = QApplication.new_local(0, {""})

local dlg = QDialog.new()
dlg:setWindowTitle("calc")

local edit = QTextEdit.new(dlg)
local vlayout = QVBoxLayout.new(dlg)
vlayout:addWidget(edit)

local gridLayout = QGridLayout.new()
vlayout:addLayout(gridLayout)

local btnText =
{
	"7", "8", "9", "/",
	"4", "5", "6", "*",
	"1", "2", "3", "-",
	"0", "-", ".", "+",
	"(", ")", "^", "%",
}

for i = 1, #btnText do
	local btn = QPushButton.new(dlg)
	btn:setText(btnText[i])
	gridLayout:addWidget(btn, math.floor((i - 1) / 4), ((i - 1) % 4))
	function btn:mouseReleaseEvent(e)
		edit:setText(edit:toPlainText():append(btn:text()))
	end
end

do
	local btn = QPushButton.new(dlg)
	btn:setText("=")
	gridLayout:addWidget(btn, 5, 0)
	function btn:mouseReleaseEvent(e)
		local s = edit:toPlainText():toStdString()
		local _, msg = pcall(loadstring("return " .. s))
		edit:setText(tostring(msg))
	end

	btn = QPushButton.new(dlg)
	btn:setText("c")
	gridLayout:addWidget(btn, 5, 1)
	function btn:mouseReleaseEvent(e)
		edit:setText("")
	end
end

function dlg:keyReleaseEvent(e)
	if e:key() == Qt.Key.Key_Return then
		local s = edit:toPlainText():toStdString()
		local _, msg = pcall(loadstring("return " .. s))
		edit:setText(tostring(msg))
	end
end

dlg:show()

app.exec()
