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

class GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $issueDisplayName;
  /**
   * @var string
   */
  public $issueId;
  /**
   * @var string
   */
  public $issueModelId;

  /**
   * @param string
   */
  public function setIssueDisplayName($issueDisplayName)
  {
    $this->issueDisplayName = $issueDisplayName;
  }
  /**
   * @return string
   */
  public function getIssueDisplayName()
  {
    return $this->issueDisplayName;
  }
  /**
   * @param string
   */
  public function setIssueId($issueId)
  {
    $this->issueId = $issueId;
  }
  /**
   * @return string
   */
  public function getIssueId()
  {
    return $this->issueId;
  }
  /**
   * @param string
   */
  public function setIssueModelId($issueModelId)
  {
    $this->issueModelId = $issueModelId;
  }
  /**
   * @return string
   */
  public function getIssueModelId()
  {
    return $this->issueModelId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1DimensionIssueDimensionMetadata');
