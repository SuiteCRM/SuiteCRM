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

namespace Google\Service\Tasks;

class AssignmentInfo extends \Google\Model
{
  protected $driveResourceInfoType = DriveResourceInfo::class;
  protected $driveResourceInfoDataType = '';
  /**
   * @var string
   */
  public $linkToTask;
  protected $spaceInfoType = SpaceInfo::class;
  protected $spaceInfoDataType = '';
  /**
   * @var string
   */
  public $surfaceType;

  /**
   * @param DriveResourceInfo
   */
  public function setDriveResourceInfo(DriveResourceInfo $driveResourceInfo)
  {
    $this->driveResourceInfo = $driveResourceInfo;
  }
  /**
   * @return DriveResourceInfo
   */
  public function getDriveResourceInfo()
  {
    return $this->driveResourceInfo;
  }
  /**
   * @param string
   */
  public function setLinkToTask($linkToTask)
  {
    $this->linkToTask = $linkToTask;
  }
  /**
   * @return string
   */
  public function getLinkToTask()
  {
    return $this->linkToTask;
  }
  /**
   * @param SpaceInfo
   */
  public function setSpaceInfo(SpaceInfo $spaceInfo)
  {
    $this->spaceInfo = $spaceInfo;
  }
  /**
   * @return SpaceInfo
   */
  public function getSpaceInfo()
  {
    return $this->spaceInfo;
  }
  /**
   * @param string
   */
  public function setSurfaceType($surfaceType)
  {
    $this->surfaceType = $surfaceType;
  }
  /**
   * @return string
   */
  public function getSurfaceType()
  {
    return $this->surfaceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AssignmentInfo::class, 'Google_Service_Tasks_AssignmentInfo');
