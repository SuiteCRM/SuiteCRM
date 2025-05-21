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

namespace Google\Service\PolicySimulator;

class GoogleCloudPolicysimulatorV1ListOrgPolicyViolationsPreviewsResponse extends \Google\Collection
{
  protected $collection_key = 'orgPolicyViolationsPreviews';
  /**
   * @var string
   */
  public $nextPageToken;
  protected $orgPolicyViolationsPreviewsType = GoogleCloudPolicysimulatorV1OrgPolicyViolationsPreview::class;
  protected $orgPolicyViolationsPreviewsDataType = 'array';

  /**
   * @param string
   */
  public function setNextPageToken($nextPageToken)
  {
    $this->nextPageToken = $nextPageToken;
  }
  /**
   * @return string
   */
  public function getNextPageToken()
  {
    return $this->nextPageToken;
  }
  /**
   * @param GoogleCloudPolicysimulatorV1OrgPolicyViolationsPreview[]
   */
  public function setOrgPolicyViolationsPreviews($orgPolicyViolationsPreviews)
  {
    $this->orgPolicyViolationsPreviews = $orgPolicyViolationsPreviews;
  }
  /**
   * @return GoogleCloudPolicysimulatorV1OrgPolicyViolationsPreview[]
   */
  public function getOrgPolicyViolationsPreviews()
  {
    return $this->orgPolicyViolationsPreviews;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudPolicysimulatorV1ListOrgPolicyViolationsPreviewsResponse::class, 'Google_Service_PolicySimulator_GoogleCloudPolicysimulatorV1ListOrgPolicyViolationsPreviewsResponse');
