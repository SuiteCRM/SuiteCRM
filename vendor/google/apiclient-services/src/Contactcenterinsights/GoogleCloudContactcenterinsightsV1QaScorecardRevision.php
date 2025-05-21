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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1QaScorecardRevision extends \Google\Collection
{
  protected $collection_key = 'alternateIds';
  /**
   * @var string[]
   */
  public $alternateIds;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $name;
  protected $snapshotType = GoogleCloudContactcenterinsightsV1QaScorecard::class;
  protected $snapshotDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param string[]
   */
  public function setAlternateIds($alternateIds)
  {
    $this->alternateIds = $alternateIds;
  }
  /**
   * @return string[]
   */
  public function getAlternateIds()
  {
    return $this->alternateIds;
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
   * @param GoogleCloudContactcenterinsightsV1QaScorecard
   */
  public function setSnapshot(GoogleCloudContactcenterinsightsV1QaScorecard $snapshot)
  {
    $this->snapshot = $snapshot;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1QaScorecard
   */
  public function getSnapshot()
  {
    return $this->snapshot;
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
class_alias(GoogleCloudContactcenterinsightsV1QaScorecardRevision::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1QaScorecardRevision');
