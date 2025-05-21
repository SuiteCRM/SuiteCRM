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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2OutputResult extends \Google\Collection
{
  protected $collection_key = 'gcsResult';
  protected $bigqueryResultType = GoogleCloudRetailV2BigQueryOutputResult::class;
  protected $bigqueryResultDataType = 'array';
  protected $gcsResultType = GoogleCloudRetailV2GcsOutputResult::class;
  protected $gcsResultDataType = 'array';

  /**
   * @param GoogleCloudRetailV2BigQueryOutputResult[]
   */
  public function setBigqueryResult($bigqueryResult)
  {
    $this->bigqueryResult = $bigqueryResult;
  }
  /**
   * @return GoogleCloudRetailV2BigQueryOutputResult[]
   */
  public function getBigqueryResult()
  {
    return $this->bigqueryResult;
  }
  /**
   * @param GoogleCloudRetailV2GcsOutputResult[]
   */
  public function setGcsResult($gcsResult)
  {
    $this->gcsResult = $gcsResult;
  }
  /**
   * @return GoogleCloudRetailV2GcsOutputResult[]
   */
  public function getGcsResult()
  {
    return $this->gcsResult;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2OutputResult::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2OutputResult');
