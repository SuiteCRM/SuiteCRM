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

class GoogleAppsDriveLabelsV2SelectionLimits extends \Google\Model
{
  protected $listLimitsType = GoogleAppsDriveLabelsV2ListLimits::class;
  protected $listLimitsDataType = '';
  /**
   * @var int
   */
  public $maxChoices;
  /**
   * @var int
   */
  public $maxDeletedChoices;
  /**
   * @var int
   */
  public $maxDisplayNameLength;
  /**
   * @var int
   */
  public $maxIdLength;

  /**
   * @param GoogleAppsDriveLabelsV2ListLimits
   */
  public function setListLimits(GoogleAppsDriveLabelsV2ListLimits $listLimits)
  {
    $this->listLimits = $listLimits;
  }
  /**
   * @return GoogleAppsDriveLabelsV2ListLimits
   */
  public function getListLimits()
  {
    return $this->listLimits;
  }
  /**
   * @param int
   */
  public function setMaxChoices($maxChoices)
  {
    $this->maxChoices = $maxChoices;
  }
  /**
   * @return int
   */
  public function getMaxChoices()
  {
    return $this->maxChoices;
  }
  /**
   * @param int
   */
  public function setMaxDeletedChoices($maxDeletedChoices)
  {
    $this->maxDeletedChoices = $maxDeletedChoices;
  }
  /**
   * @return int
   */
  public function getMaxDeletedChoices()
  {
    return $this->maxDeletedChoices;
  }
  /**
   * @param int
   */
  public function setMaxDisplayNameLength($maxDisplayNameLength)
  {
    $this->maxDisplayNameLength = $maxDisplayNameLength;
  }
  /**
   * @return int
   */
  public function getMaxDisplayNameLength()
  {
    return $this->maxDisplayNameLength;
  }
  /**
   * @param int
   */
  public function setMaxIdLength($maxIdLength)
  {
    $this->maxIdLength = $maxIdLength;
  }
  /**
   * @return int
   */
  public function getMaxIdLength()
  {
    return $this->maxIdLength;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsDriveLabelsV2SelectionLimits::class, 'Google_Service_DriveLabels_GoogleAppsDriveLabelsV2SelectionLimits');
