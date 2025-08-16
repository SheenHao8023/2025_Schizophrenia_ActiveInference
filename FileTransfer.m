% 根目录路径
rootDir = 'C:\Users\XinHao\Desktop\new';

% 获取所有子文件夹
folders = dir(rootDir);
folders = folders([folders.isdir]);
folders = folders(~ismember({folders.name}, {'.','..'}));

for i = 1:length(folders)
    subDir = fullfile(rootDir, folders(i).name);
    
    %% ===== 第一步：筛选，只保留以 1_1_ 开头的文件，其他删除 =====
    matFiles = dir(fullfile(subDir, '*.mat'));
    for j = 1:length(matFiles)
        matPath = fullfile(subDir, matFiles(j).name);
        [~, fname, ext] = fileparts(matFiles(j).name);
        
        % 如果不是以 1_1_ 开头，直接删除
        if ~strncmp(fname, '1_1_', 4)
            delete(matPath);
            continue;
        end
        
        % 如果以 b 结尾，修改内部变量名 x -> y
        if endsWith(fname, 'b')
            data = load(matPath);
            if isfield(data, 'x')
                data.y = data.x;
                data = rmfield(data, 'x');
                save(matPath, '-struct', 'data');
            end
        end
    end
    
    %% ===== 第二步：删除 1_1_1 开头的文件，并把 1_1_2~9 改名为 1_1_1~8 =====
    matFiles = dir(fullfile(subDir, '*.mat'));
    for j = 1:length(matFiles)
        [~, fname, ext] = fileparts(matFiles(j).name);
        if strncmp(fname, '1_1_1', 5)
            delete(fullfile(subDir, matFiles(j).name));
        end
    end
    
    matFiles = dir(fullfile(subDir, '*.mat'));
    for j = 1:length(matFiles)
        oldName = matFiles(j).name;
        oldPath = fullfile(subDir, oldName);
        [~, fname, ext] = fileparts(oldName);
        
        if strncmp(fname, '1_1_', 4)
            parts = split(fname, '_');
            if numel(parts) >= 3
                oldNum = str2double(parts{3});
                if oldNum >= 2 && oldNum <= 9
                    newNum = oldNum - 1;
                    newName = sprintf('1_1_%d%s', newNum, fname(length(parts{1})+length(parts{2})+length(parts{3})+3:end));
                    newPath = fullfile(subDir, [newName ext]);
                    movefile(oldPath, newPath);
                end
            end
        end
    end
    
    %% ===== 第三步：最终改名为 2_<序号>_XXA/B_<五位数字>.mat =====
    matFiles = dir(fullfile(subDir, '*.mat'));
    for j = 1:length(matFiles)
        oldName = matFiles(j).name;
        oldPath = fullfile(subDir, oldName);
        [~, fname, ext] = fileparts(oldName);
        
        % 匹配 1_1_<序号>_<五位数字>[a|b]
        tokens = regexp(fname, '^1_1_(\d+)_(\d{5})([ab])$', 'tokens');
        if ~isempty(tokens)
            seqNum  = tokens{1}{1}; % 序号
            fiveNum = tokens{1}{2}; % 五位数字
            suffix  = tokens{1}{3}; % a 或 b
            
            if suffix == 'a'
                newName = sprintf('2_%s_XXA_%s%s', seqNum, fiveNum, ext);
            else
                newName = sprintf('2_%s_XXB_%s%s', seqNum, fiveNum, ext);
            end
            
            newPath = fullfile(subDir, newName);
            movefile(oldPath, newPath);
        else
            warning('文件名不符合规则: %s', oldName);
        end
    end
end
