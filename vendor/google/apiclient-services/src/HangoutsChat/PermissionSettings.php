<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\HangoutsChat;

class PermissionSettings extends \Google\Model
{
  protected $manageAppsType = PermissionSetting::class;
  protected $manageAppsDataType = '';
  protected $manageMembersAndGroupsType = PermissionSetting::class;
  protected $manageMembersAndGroupsDataType = '';
  protected $manageWebhooksType = PermissionSetting::class;
  protected $manageWebhooksDataType = '';
  protected $modifySpaceDetailsType = PermissionSetting::class;
  protected $modifySpaceDetailsDataType = '';
  protected $postMessagesType = PermissionSetting::class;
  protected $postMessagesDataType = '';
  protected $replyMessagesType = PermissionSetting::class;
  protected $replyMessagesDataType = '';
  protected $toggleHistoryType = PermissionSetting::class;
  protected $toggleHistoryDataType = '';
  protected $useAtMentionAllType = PermissionSetting::class;
  protected $useAtMentionAllDataType = '';

  /**
   * @param PermissionSetting
   */
  public function setManageApps(PermissionSetting $manageApps)
  {
    $this->manageApps = $manageApps;
  }
  /**
   * @return PermissionSetting
   */
  public function getManageApps()
  {
    return $this->manageApps;
  }
  /**
   * @param PermissionSetting
   */
  public function setManageMembersAndGroups(PermissionSetting $manageMembersAndGroups)
  {
    $this->manageMembersAndGroups = $manageMembersAndGroups;
  }
  /**
   * @return PermissionSetting
   */
  public function getManageMembersAndGroups()
  {
    return $this->manageMembersAndGroups;
  }
  /**
   * @param PermissionSetting
   */
  public function setManageWebhooks(PermissionSetting $manageWebhooks)
  {
    $this->manageWebhooks = $manageWebhooks;
  }
  /**
   * @return PermissionSetting
   */
  public function getManageWebhooks()
  {
    return $this->manageWebhooks;
  }
  /**
   * @param PermissionSetting
   */
  public function setModifySpaceDetails(PermissionSetting $modifySpaceDetails)
  {
    $this->modifySpaceDetails = $modifySpaceDetails;
  }
  /**
   * @return PermissionSetting
   */
  public function getModifySpaceDetails()
  {
    return $this->modifySpaceDetails;
  }
  /**
   * @param PermissionSetting
   */
  public function setPostMessages(PermissionSetting $postMessages)
  {
    $this->postMessages = $postMessages;
  }
  /**
   * @return PermissionSetting
   */
  public function getPostMessages()
  {
    return $this->postMessages;
  }
  /**
   * @param PermissionSetting
   */
  public function setReplyMessages(PermissionSetting $replyMessages)
  {
    $this->replyMessages = $replyMessages;
  }
  /**
   * @return PermissionSetting
   */
  public function getReplyMessages()
  {
    return $this->replyMessages;
  }
  /**
   * @param PermissionSetting
   */
  public function setToggleHistory(PermissionSetting $toggleHistory)
  {
    $this->toggleHistory = $toggleHistory;
  }
  /**
   * @return PermissionSetting
   */
  public function getToggleHistory()
  {
    return $this->toggleHistory;
  }
  /**
   * @param PermissionSetting
   */
  public function setUseAtMentionAll(PermissionSetting $useAtMentionAll)
  {
    $this->useAtMentionAll = $useAtMentionAll;
  }
  /**
   * @return PermissionSetting
   */
  public function getUseAtMentionAll()
  {
    return $this->useAtMentionAll;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PermissionSettings::class, 'Google_Service_HangoutsChat_PermissionSettings');
