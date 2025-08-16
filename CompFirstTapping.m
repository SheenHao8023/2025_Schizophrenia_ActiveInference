% 目标文件夹
dataDir = 'C:\Users\XinHao\Desktop\2025_SCZ_AI\empirical_data\behav_rawfiles\08051';
matFiles = dir(fullfile(dataDir, '*.mat'));

for i = 1:length(matFiles)
    filePath = fullfile(dataDir, matFiles(i).name);
    S = load(filePath); % 加载 mat 文件
    
    if isfield(S, 'x') || isfield(S, 'y')
        if isfield(S, 'x')
            data = S.x;
            modeLabel = 'x';   % 使用 (A)press*
            pat8 = '\(A\)press8:\s*([+-]?\d+)';
            pat7 = '\(A\)press7:\s*([+-]?\d+)';
            pat6 = '\(A\)press6:\s*([+-]?\d+)'; % x 用不到，只占位
        else
            data = S.y;
            modeLabel = 'y';   % 使用 (B)press*
            pat8 = '\(B\)press8:\s*([+-]?\d+)';
            pat7 = '\(B\)press7:\s*([+-]?\d+)';
            pat6 = '\(B\)press6:\s*([+-]?\d+)';
        end
        
        fprintf('文件: %s | 变量: %s\n', matFiles(i).name, modeLabel);

        % 遍历（从第2行开始，用到上一行）
        for n = 2:numel(data)
            % ---------- 将当前行/上一行内容尽量整合为一段文本 ----------
            % 把 entry（可能是字符串/char/嵌套cell/结构体）中能转成字符串的部分拼起来
            concatStr = "";
            entryNow = data{n};
            if ischar(entryNow) || isstring(entryNow)
                concatStr = string(entryNow);
            elseif iscell(entryNow)
                tmp = strings(0,1);
                for kk = 1:numel(entryNow)
                    e = entryNow{kk};
                    if ischar(e) || isstring(e)
                        tmp(end+1,1) = string(e);
                    end
                end
                if ~isempty(tmp)
                    concatStr = strjoin(tmp, ' | ');
                end
            elseif isstruct(entryNow)
                fns = fieldnames(entryNow);
                tmp = strings(0,1);
                for ff = 1:numel(fns)
                    v = entryNow.(fns{ff});
                    if ischar(v) || isstring(v)
                        tmp(end+1,1) = string(v);
                    end
                end
                if ~isempty(tmp)
                    concatStr = strjoin(tmp, ' | ');
                end
            end
            concatStrPrev = "";
            entryPrev = data{n-1};
            if ischar(entryPrev) || isstring(entryPrev)
                concatStrPrev = string(entryPrev);
            elseif iscell(entryPrev)
                tmp = strings(0,1);
                for kk = 1:numel(entryPrev)
                    e = entryPrev{kk};
                    if ischar(e) || isstring(e)
                        tmp(end+1,1) = string(e);
                    end
                end
                if ~isempty(tmp)
                    concatStrPrev = strjoin(tmp, ' | ');
                end
            elseif isstruct(entryPrev)
                fns = fieldnames(entryPrev);
                tmp = strings(0,1);
                for ff = 1:numel(fns)
                    v = entryPrev.(fns{ff});
                    if ischar(v) || isstring(v)
                        tmp(end+1,1) = string(v);
                    end
                end
                if ~isempty(tmp)
                    concatStrPrev = strjoin(tmp, ' | ');
                end
            end

            % ---------- 提取需要的数字并计算 ----------
            % 对 x：只算 (A)press8 - 上一行 (A)press7
            % 对 y：算 (B)press8 - 上一行 (B)press7 以及 (B)press7 - 上一行 (B)press6
            % 提取当前行 press8
            val8 = NaN;
            tok8 = regexp(char(concatStr), pat8, 'tokens', 'once');
            if ~isempty(tok8), val8 = str2double(tok8{1}); end
            
            % 提取上一行 press7
            val7prev = NaN;
            tok7p = regexp(char(concatStrPrev), pat7, 'tokens', 'once');
            if ~isempty(tok7p), val7prev = str2double(tok7p{1}); end
            
            % 计算并打印第一项差值
            if ~isnan(val8) && ~isnan(val7prev)
                if modeLabel == "x"
                    fprintf(' 行%3d: (A)press8 - 上一行(A)press7 = %g  （%g - %g）\n', n, val8 - val7prev, val8, val7prev);
                else
                    fprintf(' 行%3d: (B)press8 - 上一行(B)press7 = %g  （%g - %g）\n', n, val8 - val7prev, val8, val7prev);
                end
            end
            
            % y 的第二项：当前行 press7 - 上一行 press6
            if modeLabel == "y"
                val7 = NaN; val6prev = NaN;
                tok7 = regexp(char(concatStr), pat7, 'tokens', 'once');
                if ~isempty(tok7), val7 = str2double(tok7{1}); end
                tok6p = regexp(char(concatStrPrev), pat6, 'tokens', 'once');
                if ~isempty(tok6p), val6prev = str2double(tok6p{1}); end
                
                if ~isnan(val7) && ~isnan(val6prev)
                    fprintf(' 行%3d: (B)press7 - 上一行(B)press6 = %g  （%g - %g）\n', n, val7 - val6prev, val7, val6prev);
                end
            end
        end
        
    else
        fprintf('文件: %s 中没有变量 x 或 y\n', matFiles(i).name);
    end
    
    fprintf('----------------------------------------\n');
end
