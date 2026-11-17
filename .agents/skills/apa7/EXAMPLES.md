# APA 7th Edition — Usage Examples

> 使用 apa7 skill 时的常见场景示例。

---

## Example 1: 生成期刊文章引用

**用户输入：**
/apa7 引用这篇文章：Lee, 2023, The role of working memory in driving performance, Transportation Research Part F, volume 95, pages 112-125

**期望输出：**
Lee, S. H. (2023). The role of working memory in driving performance. *Transportation Research Part F, 95*, 112-125. https://doi.org/[DOI]

文内引用：
- (Lee, 2023) — 括号引用
- Lee (2023) — 叙述式引用
- (Lee, 2023, p. 118) — 直接引用

---

## Example 2: 中文文献引用

**用户输入：**
/apa7 引用这篇中文文章：王伟和张磊2021年在心理学报第53卷第3期第245-260页发表的文章，题目是"驾驶疲劳对注意力的影响"

**期望输出：**
Wang, W., & Zhang, L. (2021). 驾驶疲劳对注意力的影响 [The effect of driving fatigue on attention]. *Acta Psychologica Sinica, 53*(3), 245-260. https://doi.org/[DOI]

> 注意：中文作者名使用汉语拼音（姓在前，名用首字母），中文论文标题用英文翻译在方括号中补充说明。

---

## Example 3: 构建参考文献列表

**用户输入：**
/apa7 帮我用 APA 7 格式组织以下参考文献：

- 一本2020年出版的书，作者John D. Smith和Mary K. Johnson，标题是"Cognitive Psychology and Driving Behavior"，由Routledge出版
- 一篇2019年的文章，作者Chen L.，在Accident Analysis & Prevention第125卷第33-45页，标题是"Distracted driving: A meta-analysis"

**期望输出：**
**References**

Chen, L. (2019). Distracted driving: A meta-analysis. *Accident Analysis & Prevention, 125*, 33-45. https://doi.org/10.1016/j.aap.2019.01.015

Smith, J. D., & Johnson, M. K. (2020). *Cognitive psychology and driving behavior*. Routledge.

---

## Example 4: 完整学生论文结构

**用户输入：**
/apa7 帮我写一篇 APA 7 格式学生论文的标题页

**期望输出：**
[页面顶部，居中，双倍行距]

**The Effect of Road Landscape on Driver Fatigue and Attention**

[空一行]

Jie Wang

Department of Psychology, Nanjing Normal University

PSYC 510: Advanced Cognitive Psychology

Dr. Sarah Johnson

May 22, 2026

[页码 1 在右上角]

---

## Example 5: 段落格式

**用户输入：**
/apa7 用 APA 7 格式写一段关于驾驶分心的引言

**期望输出：**
    Driver distraction is a significant contributor to traffic accidents worldwide (Lee, 2023; Wang & Zhang, 2021). Understanding the cognitive mechanisms underlying distraction is essential for developing effective countermeasures. Working memory capacity plays a crucial role in maintaining driving performance under distracting conditions (Chen, 2019). However, the relationship between working memory load and driving errors is moderated by several factors, including road complexity and individual differences in attentional control.

（首行缩进 0.5 英寸，左对齐，双倍行距）

---

## Example 6: 表格格式

**用户输入：**
/apa7 帮我按 APA 7 格式创建一个描述性统计表

**期望输出：**
**Table 1**

*Descriptive Statistics for Study Variables*

| Variable | *M* | *SD* | *n* |
|----------|-----|------|-----|
| Age (years) | 24.53 | 3.21 | 60 |
| Driving experience (years) | 5.82 | 2.45 | 60 |
| Reaction time (ms) | 452.30 | 87.65 | 60 |

*Note.* *N* = 60 participants.

---

## Example 7: 三级标题嵌套

**用户输入：**
/apa7 帮我写出 APA 7 格式的三级标题嵌套结构

**期望输出：**

**Method** （Level 1 — 居中、加粗）

**Participants** （Level 2 — 左对齐、加粗）

***Demographic Characteristics*** （Level 3 — 左对齐、加粗斜体）

...

**Materials** （Level 2 — 左对齐、加粗）

***Driving Simulator*** （Level 3 — 左对齐、加粗斜体）

---

## Example 8: 块引用（40+ word quotation）

**用户输入：**
/apa7 帮我写一个 APA 7 格式的块引用

**期望输出：**
    Previous research has demonstrated the following:
（缩进 0.5 英寸）
    Working memory capacity is a strong predictor of driving performance under dual-task conditions. Drivers with lower working memory capacity showed significantly more lane deviations and slower reaction times to critical events compared to those with higher capacity. These findings suggest that individual differences in cognitive resources play a crucial role in driving safety. (Lee, 2023, p. 120)
（块引用结束）

---

## Example 9: 统计报告

**用户输入：**
/apa7 用 APA 7 格式报告 t 检验结果

**期望输出：**
An independent-samples *t* test was conducted to compare reaction times between the high-load and low-load conditions. There was a significant difference in reaction times between the high-load (*M* = 523.45 ms, *SD* = 89.32) and low-load (*M* = 412.78 ms, *SD* = 76.54) conditions; *t*(58) = 5.12, *p* < .001, Cohen's *d* = 1.32.

---

## Example 10: 书籍章节引用

**用户输入：**
/apa7 引用一本书中的一章：作者赵明，2022年，章节标题"自动驾驶中的人因问题"，在陈伟主编的《智能交通心理学》(pp. 88-112)，由科学出版社出版

**期望输出：**
Zhao, M. (2022). 自动驾驶中的人因问题 [Human factors in autonomous driving]. In W. Chen (Ed.), *智能交通心理学* (pp. 88-112). Science Press.
