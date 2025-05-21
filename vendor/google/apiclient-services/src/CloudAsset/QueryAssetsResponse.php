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

namespace Google\Service\CloudAsset;

class QueryAssetsResponse extends \Google\Model
{
  /**
   * @var bool
   */
  public $done;
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $jobReference;
  protected $outputConfigType = QueryAssetsOutputConfig::class;
  protected $outputConfigDataType = '';
  protected $queryResultType = QueryResult::class;
  protected $queryResultDataType = '';

  /**
   * @param bool
   */
  public function setDone($done)
  {
    $this->done = $done;
  }
  /**
   * @return bool
   */
  public function getDone()
  {
    return $this->done;
  }
  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string
   */
  public function setJobReference($jobReference)
  {
    $this->jobReference = $jobReference;
  }
  /**
   * @return string
   */
  public function getJobReference()
  {
    return $this->jobReference;
  }
  /**
   * @param QueryAssetsOutputConfig
   */
  public function setOutputConfig(QueryAssetsOutputConfig $outputConfig)
  {
    $this->outputConfig = $outputConfig;
  }
  /**
   * @return QueryAssetsOutputConfig
   */
  public function getOutputConfig()
  {
    return $this->outputConfig;
  }
  /**
   * @param QueryResult
   */
  public function setQueryResult(QueryResult $queryResult)
  {
    $this->queryResult = $queryResult;
  }
  /**
   * @return QueryResult
   */
  public function getQueryResult()
  {
    return $this->queryResult;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QueryAssetsResponse::class, 'Google_Service_CloudAsset_QueryAssetsResponse');
