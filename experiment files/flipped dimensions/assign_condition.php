<?php

include('../common/jrd_database_connect.php');

$result = mysql_query('SELECT sid FROM co_ex_exp_6_subjects ORDER BY sid DESC LIMIT 1');

if($result) {
	$arr = mysql_fetch_array($result);
	$subject_id = intval($arr[0]);
	$subject_id++; 
} else {
	$subject_id = 1;
}

$condition = $subject_id % 12;

echo $condition;

?>