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

namespace Google\Service\ChecksService;

class GoogleChecksRepoScanV1alphaScmMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $branch;
  protected $pullRequestType = GoogleChecksRepoScanV1alphaPullRequest::class;
  protected $pullRequestDataType = '';
  /**
   * @var string
   */
  public $remoteUri;
  /**
   * @var string
   */
  public $revisionId;

  /**
   * @param string
   */
  public function setBranch($branch)
  {
    $this->branch = $branch;
  }
  /**
   * @return string
   */
  public function getBranch()
  {
    return $this->branch;
  }
  /**
   * @param GoogleChecksRepoScanV1alphaPullRequest
   */
  public function setPullRequest(GoogleChecksRepoScanV1alphaPullRequest $pullRequest)
  {
    $this->pullRequest = $pullRequest;
  }
  /**
   * @return GoogleChecksRepoScanV1alphaPullRequest
   */
  public function getPullRequest()
  {
    return $this->pullRequest;
  }
  /**
   * @param string
   */
  public function setRemoteUri($remoteUri)
  {
    $this->remoteUri = $remoteUri;
  }
  /**
   * @return string
   */
  public function getRemoteUri()
  {
    return $this->remoteUri;
  }
  /**
   * @param string
   */
  public function setRevisionId($revisionId)
  {
    $this->revisionId = $revisionId;
  }
  /**
   * @return string
   */
  public function getRevisionId()
  {
    return $this->revisionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChecksRepoScanV1alphaScmMetadata::class, 'Google_Service_ChecksService_GoogleChecksRepoScanV1alphaScmMetadata');
