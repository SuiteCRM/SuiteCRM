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

namespace Google\Service\DriveLabels;

class GoogleAppsDriveLabelsV2LabelLock extends \Google\Model
{
  protected $capabilitiesType = GoogleAppsDriveLabelsV2LabelLockCapabilities::class;
  protected $capabilitiesDataType = '';
  /**
   * @var string
   */
  public $choiceId;
  /**
   * @var string
   */
  public $createTime;
  protected $creatorType = GoogleAppsDriveLabelsV2UserInfo::class;
  protected $creatorDataType = '';
  /**
   * @var string
   */
  public $deleteTime;
  /**
   * @var string
   */
  public $fieldId;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;

  /**
   * @param GoogleAppsDriveLabelsV2LabelLockCapabilities
   */
  public function setCapabilities(GoogleAppsDriveLabelsV2LabelLockCapabilities $capabilities)
  {
    $this->capabilities = $capabilities;
  }
  /**
   * @return GoogleAppsDriveLabelsV2LabelLockCapabilities
   */
  public function getCapabilities()
  {
    return $this->capabilities;
  }
  /**
   * @param string
   */
  public function setChoiceId($choiceId)
  {
    $this->choiceId = $choiceId;
  }
  /**
   * @return string
   */
  public function getChoiceId()
  {
    return $this->choiceId;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param GoogleAppsDriveLabelsV2UserInfo
   */
  public function setCreator(GoogleAppsDriveLabelsV2UserInfo $creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return GoogleAppsDriveLabelsV2UserInfo
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param string
   */
  public function setDeleteTime($deleteTime)
  {
    $this->deleteTime = $deleteTime;
  }
  /**
   * @return string
   */
  public function getDeleteTime()
  {
    return $this->deleteTime;
  }
  /**
   * @param string
   */
  public function setFieldId($fieldId)
  {
    $this->fieldId = $fieldId;
  }
  /**
   * @return string
   */
  public function getFieldId()
  {
    return $this->fieldId;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsDriveLabelsV2LabelLock::class, 'Google_Service_DriveLabels_GoogleAppsDriveLabelsV2LabelLock');
