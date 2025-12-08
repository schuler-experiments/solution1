
# Task Manager - Notification & Alert System

## Overview

The Notification & Alert System is a comprehensive notification management layer that provides multi-channel notification delivery, user preferences, templating, escalation rules, and digest notifications for the task manager.

## Features

### 1. Core Notification Management

#### Notification Types
- **Email** - Email notifications (simulated)
- **SMS** - SMS text messages (simulated)
- **Push** - Push notifications (simulated)
- **In-App** - In-application notifications
- **Desktop** - Desktop system notifications (simulated)
- **Slack** - Slack channel messages (simulated)
- **Webhook** - HTTP webhook calls (simulated)

#### Notification Priorities
- **Low** - Non-urgent information
- **Normal** - Standard notifications
- **High** - Important notifications
- **Urgent** - Critical alerts requiring immediate attention

#### Notification Statuses
- **Scheduled** - Notification is scheduled for future delivery
- **Pending** - Notification is queued for sending
- **Sent** - Notification has been delivered
- **Failed** - Delivery failed
- **Read** - Notification has been read by recipient
- **Dismissed** - Notification has been dismissed

### 2. Notification Templates

Create reusable notification templates with variable substitution:

```pascal
templateID := CreateTemplate(
  'Task Due Reminder',
  'Remind user when task is due',
  ntEmail,
  'Task Due: {TASK_TITLE}',
  'Your {TASK_PRIORITY} priority task "{TASK_TITLE}" is due soon.',
  npNormal
);
```

**Template Variables:**
- `{TASK_TITLE}` - Task title
- `{TASK_DESCRIPTION}` - Task description
- `{TASK_PRIORITY}` - Task priority (High, Medium, Low)
- `{TASK_STATUS}` - Task status
- `{TASK_CATEGORY}` - Task category

### 3. User Notification Preferences

Users can customize their notification experience:

```pascal
SetUserPreference(
  userID,
  ntEmail,
  true,           // Enabled
  22,             // Quiet hours start (10 PM)
  8,              // Quiet hours end (8 AM)
  dfDaily         // Daily digest frequency
);
```

**Preference Options:**
- **Quiet Hours** - Suppress notifications during specified hours
- **Digest Frequency** - Combine notifications (None, Hourly, Daily, Weekly)
- **Preferred Channel** - Default delivery channel
- **Weekend Notifications** - Allow/block weekend notifications
- **Nighttime Notifications** - Allow/block notifications between 10 PM - 6 AM
- **Minimum Priority** - Only receive notifications above certain priority

### 4. Escalation Rules

Automatically escalate important tasks with customizable rules:

```pascal
ruleID := CreateEscalationRule(
  'High Priority Escalation',
  'Escalate high priority tasks after 30 minutes',
  tpHigh,         // Task priority to escalate
  30,             // Initial delay in minutes
  15,             // Escalation interval in minutes
  3,              // Maximum escalations
  dcEmail         // Escalation channel
);
```

### 5. Digest Notifications

Group multiple notifications into periodic digests:

```pascal
digestID := CreateDigest(userID, dfDaily);
```

**Digest Frequencies:**
- **Hourly** - Every hour
- **Daily** - Once per day
- **Weekly** - Once per week

### 6. Notification Scheduling

Schedule notifications for future delivery:

```pascal
notifID := CreateNotification(
  ntEmail,
  npHigh,
  taskID,
  recipientID,
  'Task Reminder',
  'Your task is due tomorrow',
  IncDay(Now, 1)  // Schedule for tomorrow
);
```

### 7. Bulk Operations

Send notifications to multiple recipients:

```pascal
count := SendBulkNotification(
  [user1, user2, user3],
  'Team Update',
  'All tasks have been updated',
  npNormal
);
```

### 8. Notification Queries

Retrieve notifications by various criteria:

```pascal
// Get all notifications for a task
notifs := GetNotificationsByTask(taskID);

// Get unread notifications
notifs := GetUnreadNotifications(userID);

// Get notifications by priority
notifs := GetNotificationsByPriority(npHigh);

// Get recent notifications
notifs := GetRecentNotifications(userID, 24); // Last 24 hours
```

### 9. Statistics & Reporting

Track notification performance:

```pascal
stats := GetNotificationStatistics;
// Returns:
// - TotalSent
// - TotalFailed
// - TotalRead
// - TotalDismissed
// - AverageReadTime
// - Breakdown by type
// - Breakdown by priority

successRate := GetDeliverySuccessRate;
report := GenerateNotificationReport;
```

### 10. Smart Delivery Rules

The system intelligently manages notification delivery:

- **Quiet Hours** - Respects user quiet hours
- **Weekend Protection** - Optional weekend notification blocking
- **Nighttime Protection** - Optional nighttime notification blocking
- **Priority Override** - Urgent notifications bypass quiet hours
- **Delivery Retry** - Automatic retry for failed notifications
- **Duplicate Prevention** - Prevents duplicate notifications

## Usage Examples

### Example 1: Task Due Notification

```pascal
// Create a notification when task is due soon
notifID := CreateNotification(
  ntEmail,
  npHigh,
  taskID,
  userID,
  'Task Due in 24 Hours',
  'Your task "' + taskTitle + '" is due tomorrow',
  Now
);

if SendNotification(notifID) then
  WriteLn('Notification sent successfully');
```

### Example 2: Using Templates

```pascal
// Create template
templateID := CreateTemplate(
  'Task Assigned',
  'Notify when task is assigned',
  ntPush,
  'New Task: {TASK_TITLE}',
  'You have been assigned to {TASK_TITLE} with {TASK_PRIORITY} priority',
  npNormal
);

// Use template
notifID := CreateNotificationFromTemplate(
  templateID,
  taskID,
  userID,
  Now
);
```

### Example 3: Setting Up User Preferences

```pascal
// Configure user notification preferences
SetUserPreference(userID, ntEmail, true, 22, 8, dfDaily);
SetUserPreference(userID, ntPush, true, 0, 0, dfNone);
SetUserPreference(userID, ntSMS, false, 0, 0, dfNone);

// Update specific settings
UpdateQuietHours(userID, ntEmail, 23, 7);
SetDigestFrequency(userID, dfWeekly);
```

### Example 4: Escalation for Overdue Tasks

```pascal
// Create escalation rule for critical tasks
ruleID := CreateEscalationRule(
  'Critical Task Escalation',
  'Escalate critical tasks immediately',
  tpCritical,
  0,              // No initial delay
  10,             // Escalate every 10 minutes
  5,              // Up to 5 escalations
  dcAll           // Use all channels
);
```

### Example 5: Daily Digest

```pascal
// Set up daily digest for user
digestID := CreateDigest(userID, dfDaily);

// Digest will automatically collect and send notifications
// at the scheduled time
```

## API Reference

### Notification Creation & Management

- `CreateNotification(type, priority, taskID, recipientID, title, message, scheduledTime): Integer`
- `SendNotification(notificationID): Boolean`
- `ScheduleNotification(notificationID, scheduledTime): Boolean`
- `CancelNotification(notificationID): Boolean`
- `MarkAsRead(notificationID): Boolean`
- `DismissNotification(notificationID): Boolean`
- `RetryFailedNotification(notificationID): Boolean`

### Template Management

- `CreateTemplate(name, description, type, titleTemplate, messageTemplate, priority): Integer`
- `UpdateTemplate(templateID, titleTemplate, messageTemplate): Boolean`
- `DeleteTemplate(templateID): Boolean`
- `ActivateTemplate(templateID): Boolean`
- `DeactivateTemplate(templateID): Boolean`
- `GetAllTemplates: TNotificationTemplateArray`
- `CreateNotificationFromTemplate(templateID, taskID, recipientID, scheduledTime): Integer`

### Preference Management

- `SetUserPreference(userID, type, enabled, quietStart, quietEnd, digestFreq): Integer`
- `GetUserPreference(userID, type): TNotificationPreference`
- `UpdateQuietHours(userID, type, startHour, endHour): Boolean`
- `EnableAllNotifications(userID): Boolean`
- `DisableAllNotifications(userID): Boolean`
- `SetDigestFrequency(userID, frequency): Boolean`

### Escalation Management

- `CreateEscalationRule(name, description, taskPriority, initialDelay, interval, maxEscalations, channel): Integer`
- `DeleteEscalationRule(ruleID): Boolean`
- `ActivateEscalationRule(ruleID): Boolean`
- `DeactivateEscalationRule(ruleID): Boolean`
- `GetAllEscalationRules: TEscalationRuleArray`
- `TriggerEscalation(taskID): Boolean`

### Digest Management

- `CreateDigest(recipientID, frequency): Integer`
- `UpdateDigestSchedule(digestID, frequency): Boolean`
- `SendDigest(digestID): Boolean`
- `GetPendingDigests: TDigestNotificationArray`

### Query Functions

- `GetNotificationsByTask(taskID): TNotificationArray`
- `GetNotificationsByRecipient(recipientID): TNotificationArray`
- `GetUnreadNotifications(recipientID): TNotificationArray`
- `GetFailedNotifications: TNotificationArray`
- `GetScheduledNotifications: TNotificationArray`
- `GetNotificationsByPriority(priority): TNotificationArray`
- `GetNotificationsByStatus(status): TNotificationArray`
- `GetRecentNotifications(recipientID, hours): TNotificationArray`

### Statistics & Reporting

- `GetNotificationStatistics: TNotificationStats`
- `GetUserNotificationStats(recipientID): TNotificationStats`
- `GetDeliverySuccessRate: Double`
- `GetAverageReadTime: Double`
- `GenerateNotificationReport: string`

### Bulk Operations

- `SendBulkNotification(recipientIDs, title, message, priority): Integer`
- `DeleteOldNotifications(daysOld): Integer`
- `RetryAllFailed: Integer`

### Processing

- `ProcessScheduledNotifications`
- `ProcessAllPending`

### Configuration

- `EnableNotifications(enabled: Boolean)`
- `SetDefaultRetries(retries: Integer)`
- `GetNotificationsEnabled: Boolean`

### Persistence

- `SaveNotificationDataToFile(filename): Boolean`
- `LoadNotificationDataFromFile(filename): Boolean`

## Data Structures

### TNotification
- ID, NotificationType, Priority, Status
- TaskID, RecipientID
- Title, Message
- ScheduledTime, SentTime, ReadTime
- Channel, RetryCount, MaxRetries
- Metadata, CreatedAt

### TNotificationTemplate
- ID, Name, Description
- NotificationType, Priority
- TitleTemplate, MessageTemplate
- IsActive, CreatedAt

### TNotificationPreference
- ID, UserID, NotificationType
- IsEnabled
- QuietHoursStart, QuietHoursEnd
- DigestFrequency, PreferredChannel
- AllowWeekends, AllowNighttime
- MinPriority

### TEscalationRule
- ID, Name, Description
- TaskPriority
- InitialDelay, EscalationInterval, MaxEscalations
- EscalateChannel, IsActive

### TNotificationStats
- TotalSent, TotalFailed, TotalRead, TotalDismissed
- AverageReadTime
- ByType[TNotificationType]
- ByPriority[TNotificationPriority]

## Integration with Other Features

The notification system integrates seamlessly with:

- **Reminder System** - Automatic notifications when reminders trigger
- **Workflow System** - Notifications for workflow events
- **Team Collaboration** - Notifications for task assignments and updates
- **Smart Features** - Notifications for risk assessments and anomalies
- **Wellbeing System** - Notifications for break reminders and burnout alerts

## Best Practices

1. **Use Templates** - Create reusable templates for common notification types
2. **Respect User Preferences** - Always honor quiet hours and user settings
3. **Prioritize Appropriately** - Use urgent priority sparingly
4. **Batch Notifications** - Use digests for non-urgent updates
5. **Monitor Statistics** - Track delivery rates and user engagement
6. **Clean Old Data** - Regularly delete old notifications
7. **Test Escalations** - Verify escalation rules work as expected
8. **Provide Opt-Out** - Allow users to disable notification types they don't need

## Performance Considerations

- Notifications are stored in memory (dynamic arrays)
- Use `DeleteOldNotifications` to manage memory
- Digest notifications reduce notification volume
- Template substitution is performed at send time
- Statistics are calculated on-demand

## Future Enhancements

Potential future additions:
- Real email/SMS integration
- Webhook implementation
- Push notification services integration
- Rich media support (images, attachments)
- Two-way communication (reply to notifications)
- Notification threading (conversation view)
- Machine learning for optimal send times
- A/B testing for notification effectiveness

## Testing

The `SelfTest` procedure demonstrates all major features:
- Notification creation and sending
- Template usage
- User preferences
- Escalation rules
- Digest notifications
- Statistics and reporting
- Bulk operations

Run the test with:
```bash
fpc solution20.pas -obin/notification_manager -O1 -Mobjfpc -Fusolution1
bin/notification_manager
```

## Conclusion

The Notification & Alert System provides enterprise-grade notification management for the task manager, with support for multiple channels, user preferences, smart delivery rules, and comprehensive tracking. It enhances user engagement while respecting user preferences and preventing notification fatigue.
