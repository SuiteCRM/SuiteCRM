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

namespace Google\Service\Looker;

class UserMetadata extends \Google\Model
{
  /**
   * @var int
   */
  public $additionalDeveloperUserCount;
  /**
   * @var int
   */
  public $additionalStandardUserCount;
  /**
   * @var int
   */
  public $additionalViewerUserCount;

  /**
   * @param int
   */
  public function setAdditionalDeveloperUserCount($additionalDeveloperUserCount)
  {
    $this->additionalDeveloperUserCount = $additionalDeveloperUserCount;
  }
  /**
   * @return int
   */
  public function getAdditionalDeveloperUserCount()
  {
    return $this->additionalDeveloperUserCount;
  }
  /**
   * @param int
   */
  public function setAdditionalStandardUserCount($additionalStandardUserCount)
  {
    $this->additionalStandardUserCount = $additionalStandardUserCount;
  }
  /**
   * @return int
   */
  public function getAdditionalStandardUserCount()
  {
    return $this->additionalStandardUserCount;
  }
  /**
   * @param int
   */
  public function setAdditionalViewerUserCount($additionalViewerUserCount)
  {
    $this->additionalViewerUserCount = $additionalViewerUserCount;
  }
  /**
   * @return int
   */
  public function getAdditionalViewerUserCount()
  {
    return $this->additionalViewerUserCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UserMetadata::class, 'Google_Service_Looker_UserMetadata');
