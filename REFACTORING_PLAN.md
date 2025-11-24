
# Refactoring Plan for Files Exceeding 500-Line Limit

## Current Status
- **TaskStakeholder.pas**: 757 lines (exceeds by 257 lines)
- **TaskWorkflow.pas**: 703 lines (exceeds by 203 lines)
- **TaskKnowledgeBase.pas**: 611 lines (exceeds by 111 lines)

## Refactoring Strategy

### Phase 1: TaskStakeholder.pas (757 → ~400 + ~350)
**Split into:**
1. **TaskStakeholder.pas** (Core) - ~400 lines
   - Keep: TStakeholder type definition
   - Keep: TTaskStakeholderManagerClass with core CRUD operations
   - Keep: Basic engagement management methods
   - Remove: Interaction logging and retrieval methods

2. **TaskStakeholderInteractions.pas** (New) - ~350 lines
   - New helper class: TStakeholderInteractionManager
   - Move: All interaction-related functionality
   - Move: LogInteraction, GetStakeholderInteractions, GetRecentInteractions, GetInteractionCount

### Phase 2: TaskWorkflow.pas (703 → ~400 + ~300)
**Split into:**
1. **TaskWorkflow.pas** (Core) - ~400 lines
   - Keep: Core workflow rule and transition management
   - Keep: Template management
   - Remove: Analysis and statistics methods

2. **TaskWorkflowAnalysis.pas** (New) - ~300 lines
   - New helper class: TWorkflowAnalyzer
   - Move: GetAverageTimeInState, GetMostCommonTransition, GetWorkflowStats, GetStateDistribution

### Phase 3: TaskKnowledgeBase.pas (611 → ~350 + ~250)
**Split into:**
1. **TaskKnowledgeBase.pas** (Core) - ~350 lines
   - Keep: Basic CRUD operations for articles
   - Keep: Tag management
   - Remove: Advanced search and analysis

2. **TaskKnowledgeBaseAnalysis.pas** (New) - ~250 lines
   - New helper class: TKnowledgeBaseAnalyzer
   - Move: Advanced search, statistics, and reporting

## Implementation Order
1. Create new units for interaction/analysis functionality
2. Refactor original units by removing methods moved to new units
3. Compile and test after each phase
4. Git commit after each successful phase
5. Update documentation

## Backward Compatibility
- All public APIs remain the same
- New units provide same functionality through separate classes
- No breaking changes to existing code
