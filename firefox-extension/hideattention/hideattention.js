const hideAllElements = (item) => {
  item.hidden = true;
};

document.querySelectorAll('.ProfileTweet-actionList').forEach(hideAllElements);
document.querySelectorAll('.ProfileNav-item--followers').forEach(hideAllElements);
document.querySelectorAll('.ProfileNav-item--following').forEach(hideAllElements);
document.querySelectorAll('.ProfileNav-item--tweets').forEach(hideAllElements);
document.querySelectorAll('.ProfileNav-item--favorites').forEach(hideAllElements);