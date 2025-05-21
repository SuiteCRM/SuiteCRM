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

class GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsMetadata extends \Google\Collection
{
  protected $collection_key = 'partialErrors';
  /**
   * @var int
   */
  public $completedAnalysesCount;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var int
   */
  public $failedAnalysesCount;
  protected $partialErrorsType = GoogleRpcStatus::class;
  protected $partialErrorsDataType = 'array';
  protected $requestType = GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest::class;
  protected $requestDataType = '';
  /**
   * @var int
   */
  public $totalRequestedAnalysesCount;

  /**
   * @param int
   */
  public function setCompletedAnalysesCount($completedAnalysesCount)
  {
    $this->completedAnalysesCount = $completedAnalysesCount;
  }
  /**
   * @return int
   */
  public function getCompletedAnalysesCount()
  {
    return $this->completedAnalysesCount;
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
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param int
   */
  public function setFailedAnalysesCount($failedAnalysesCount)
  {
    $this->failedAnalysesCount = $failedAnalysesCount;
  }
  /**
   * @return int
   */
  public function getFailedAnalysesCount()
  {
    return $this->failedAnalysesCount;
  }
  /**
   * @param GoogleRpcStatus[]
   */
  public function setPartialErrors($partialErrors)
  {
    $this->partialErrors = $partialErrors;
  }
  /**
   * @return GoogleRpcStatus[]
   */
  public function getPartialErrors()
  {
    return $this->partialErrors;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest
   */
  public function setRequest(GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest $request)
  {
    $this->request = $request;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsRequest
   */
  public function getRequest()
  {
    return $this->request;
  }
  /**
   * @param int
   */
  public function setTotalRequestedAnalysesCount($totalRequestedAnalysesCount)
  {
    $this->totalRequestedAnalysesCount = $totalRequestedAnalysesCount;
  }
  /**
   * @return int
   */
  public function getTotalRequestedAnalysesCount()
  {
    return $this->totalRequestedAnalysesCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsMetadata::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1BulkAnalyzeConversationsMetadata');
