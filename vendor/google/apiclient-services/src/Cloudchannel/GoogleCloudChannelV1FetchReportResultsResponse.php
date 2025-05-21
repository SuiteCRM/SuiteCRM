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

namespace Google\Service\Cloudchannel;

class GoogleCloudChannelV1FetchReportResultsResponse extends \Google\Collection
{
  protected $collection_key = 'rows';
  /**
   * @var string
   */
  public $nextPageToken;
  protected $reportMetadataType = GoogleCloudChannelV1ReportResultsMetadata::class;
  protected $reportMetadataDataType = '';
  protected $rowsType = GoogleCloudChannelV1Row::class;
  protected $rowsDataType = 'array';

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
   * @param GoogleCloudChannelV1ReportResultsMetadata
   */
  public function setReportMetadata(GoogleCloudChannelV1ReportResultsMetadata $reportMetadata)
  {
    $this->reportMetadata = $reportMetadata;
  }
  /**
   * @return GoogleCloudChannelV1ReportResultsMetadata
   */
  public function getReportMetadata()
  {
    return $this->reportMetadata;
  }
  /**
   * @param GoogleCloudChannelV1Row[]
   */
  public function setRows($rows)
  {
    $this->rows = $rows;
  }
  /**
   * @return GoogleCloudChannelV1Row[]
   */
  public function getRows()
  {
    return $this->rows;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudChannelV1FetchReportResultsResponse::class, 'Google_Service_Cloudchannel_GoogleCloudChannelV1FetchReportResultsResponse');
