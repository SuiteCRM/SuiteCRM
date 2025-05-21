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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1BatchPredictionJobOutputInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $bigqueryOutputDataset;
  /**
   * @var string
   */
  public $bigqueryOutputTable;
  /**
   * @var string
   */
  public $gcsOutputDirectory;

  /**
   * @param string
   */
  public function setBigqueryOutputDataset($bigqueryOutputDataset)
  {
    $this->bigqueryOutputDataset = $bigqueryOutputDataset;
  }
  /**
   * @return string
   */
  public function getBigqueryOutputDataset()
  {
    return $this->bigqueryOutputDataset;
  }
  /**
   * @param string
   */
  public function setBigqueryOutputTable($bigqueryOutputTable)
  {
    $this->bigqueryOutputTable = $bigqueryOutputTable;
  }
  /**
   * @return string
   */
  public function getBigqueryOutputTable()
  {
    return $this->bigqueryOutputTable;
  }
  /**
   * @param string
   */
  public function setGcsOutputDirectory($gcsOutputDirectory)
  {
    $this->gcsOutputDirectory = $gcsOutputDirectory;
  }
  /**
   * @return string
   */
  public function getGcsOutputDirectory()
  {
    return $this->gcsOutputDirectory;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1BatchPredictionJobOutputInfo::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1BatchPredictionJobOutputInfo');
