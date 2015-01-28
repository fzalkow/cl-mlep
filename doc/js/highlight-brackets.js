/* Highlight matching brackets
    Frank Zalkow, 2014  */
    
$(document).ready(function() {
    var tmpNext=null;
    $("code.lisp span.list").hover(
        function() {
            
            tmpNext = $(this);
            var startChar = tmpNext.text()
            var endChar = (startChar == "(" ? ")" : "(");
            
            var closing_needed = 1;
            
            while (true) {
                
                tmpNext = (startChar == "(" ? tmpNext.next() : tmpNext.prev());
                if (tmpNext.prop("tagName") == "p") {break;} // exceeded the code block
                if (tmpNext.hasClass("list")) {
                    if (tmpNext.text() == startChar) {closing_needed++;}
                    else if (tmpNext.text() == endChar) {closing_needed--;}
                    if (closing_needed == 0) {break;}
                };
                
            };
            
            
            $(this).addClass("active");
            tmpNext.addClass("active");
        },
        function() {
            $(this).removeClass("active");
            tmpNext.removeClass("active");
        })
});