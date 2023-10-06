classdef UserComment < handle
    properties (Constant)
        USER_COMMENT_PATTERN = '(?<=^\n*%%+)[^%][^\n]+'
    end
    
    
    
    
    methods (Static)
        function parse(p)
            import parser.UserComment
            match = regexp( ...
                p.Code, ...
                UserComment.USER_COMMENT_PATTERN, ...
                'match','once' ...
            );
            p.UserComment = strtrim(match);
        end%
    end
end
        
