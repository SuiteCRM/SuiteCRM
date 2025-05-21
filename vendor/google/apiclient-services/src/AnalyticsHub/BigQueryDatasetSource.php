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

namespace Google\Service\AnalyticsHub;

class BigQueryDatasetSource extends \Google\Collection
{
  protected $collection_key = 'selectedResources';
  /**
   * @var string
   */
  public $dataset;
  protected $restrictedExportPolicyType = RestrictedExportPolicy::class;
  protected $restrictedExportPolicyDataType = '';
  protected $selectedResourcesType = SelectedResource::class;
  protected $selectedResourcesDataType = 'array';

  /**
   * @param string
   */
  public function setDataset($dataset)
  {
    $this->dataset = $dataset;
  }
  /**
   * @return string
   */
  public function getDataset()
  {
    return $this->dataset;
  }
  /**
   * @param RestrictedExportPolicy
   */
  public function setRestrictedExportPolicy(RestrictedExportPolicy $restrictedExportPolicy)
  {
    $this->restrictedExportPolicy = $restrictedExportPolicy;
  }
  /**
   * @return RestrictedExportPolicy
   */
  public function getRestrictedExportPolicy()
  {
    return $this->restrictedExportPolicy;
  }
  /**
   * @param SelectedResource[]
   */
  public function setSelectedResources($selectedResources)
  {
    $this->selectedResources = $selectedResources;
  }
  /**
   * @return SelectedResource[]
   */
  public function getSelectedResources()
  {
    return $this->selectedResources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BigQueryDatasetSource::class, 'Google_Service_AnalyticsHub_BigQueryDatasetSource');
