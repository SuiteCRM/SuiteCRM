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

namespace Google\Service\BinaryAuthorization;

class PodResult extends \Google\Collection
{
  protected $collection_key = 'imageResults';
  protected $imageResultsType = ImageResult::class;
  protected $imageResultsDataType = 'array';
  /**
   * @var string
   */
  public $kubernetesNamespace;
  /**
   * @var string
   */
  public $kubernetesServiceAccount;
  /**
   * @var string
   */
  public $podName;
  /**
   * @var string
   */
  public $verdict;

  /**
   * @param ImageResult[]
   */
  public function setImageResults($imageResults)
  {
    $this->imageResults = $imageResults;
  }
  /**
   * @return ImageResult[]
   */
  public function getImageResults()
  {
    return $this->imageResults;
  }
  /**
   * @param string
   */
  public function setKubernetesNamespace($kubernetesNamespace)
  {
    $this->kubernetesNamespace = $kubernetesNamespace;
  }
  /**
   * @return string
   */
  public function getKubernetesNamespace()
  {
    return $this->kubernetesNamespace;
  }
  /**
   * @param string
   */
  public function setKubernetesServiceAccount($kubernetesServiceAccount)
  {
    $this->kubernetesServiceAccount = $kubernetesServiceAccount;
  }
  /**
   * @return string
   */
  public function getKubernetesServiceAccount()
  {
    return $this->kubernetesServiceAccount;
  }
  /**
   * @param string
   */
  public function setPodName($podName)
  {
    $this->podName = $podName;
  }
  /**
   * @return string
   */
  public function getPodName()
  {
    return $this->podName;
  }
  /**
   * @param string
   */
  public function setVerdict($verdict)
  {
    $this->verdict = $verdict;
  }
  /**
   * @return string
   */
  public function getVerdict()
  {
    return $this->verdict;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PodResult::class, 'Google_Service_BinaryAuthorization_PodResult');
