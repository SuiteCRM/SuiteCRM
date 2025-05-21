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

namespace Google\Service\Datastore;

class RunAggregationQueryResponse extends \Google\Model
{
  protected $batchType = AggregationResultBatch::class;
  protected $batchDataType = '';
  protected $explainMetricsType = ExplainMetrics::class;
  protected $explainMetricsDataType = '';
  protected $queryType = AggregationQuery::class;
  protected $queryDataType = '';
  /**
   * @var string
   */
  public $transaction;

  /**
   * @param AggregationResultBatch
   */
  public function setBatch(AggregationResultBatch $batch)
  {
    $this->batch = $batch;
  }
  /**
   * @return AggregationResultBatch
   */
  public function getBatch()
  {
    return $this->batch;
  }
  /**
   * @param ExplainMetrics
   */
  public function setExplainMetrics(ExplainMetrics $explainMetrics)
  {
    $this->explainMetrics = $explainMetrics;
  }
  /**
   * @return ExplainMetrics
   */
  public function getExplainMetrics()
  {
    return $this->explainMetrics;
  }
  /**
   * @param AggregationQuery
   */
  public function setQuery(AggregationQuery $query)
  {
    $this->query = $query;
  }
  /**
   * @return AggregationQuery
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param string
   */
  public function setTransaction($transaction)
  {
    $this->transaction = $transaction;
  }
  /**
   * @return string
   */
  public function getTransaction()
  {
    return $this->transaction;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RunAggregationQueryResponse::class, 'Google_Service_Datastore_RunAggregationQueryResponse');
