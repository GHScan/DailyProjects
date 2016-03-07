<?php 
	function l2u($s) { return iconv('gb18030', 'utf-8', $s); }
	function u2l($s) { return iconv('utf-8', 'gb18030', $s); }
	function _mime_content_type($path) {
	    $finfo = finfo_open(FILEINFO_MIME_TYPE);
		$type = finfo_file($finfo, $path);
		finfo_close($finfo);
		return $type;
	}

	$path = isset($_GET['path']) ? $_GET['path'] : 'D:\\';

	if (is_file(u2l($path))) {
		
		header("Content-type: " . _mime_content_type(u2l($path)));
		echo file_get_contents(u2l($path));

	} else {
		?>			

		<html>
		<head>
			<title>content of <?php echo $path ?> </title>
		</head>
		<body>
			<?php 
			
				echo "<ul data-role='listview'>";
				foreach (scandir(u2l($path)) as $entry) {
					$newEntry = l2u($entry);
					$subPath = urlencode($path . '\\' . $newEntry);
					echo "<li><a href=\"?path=$subPath\">$newEntry</li>";
				}
				echo "</ul>";

			?>
		</body>
		</html>

		<?php
	}
?>