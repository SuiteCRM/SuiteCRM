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

namespace Google\Service\Aiplatform\Resource;

use Google\Service\Aiplatform\GoogleCloudAiplatformV1Dataset;
use Google\Service\Aiplatform\GoogleCloudAiplatformV1ExportDataRequest;
use Google\Service\Aiplatform\GoogleCloudAiplatformV1ImportDataRequest;
use Google\Service\Aiplatform\GoogleCloudAiplatformV1ListDatasetsResponse;
use Google\Service\Aiplatform\GoogleCloudAiplatformV1SearchDataItemsResponse;
use Google\Service\Aiplatform\GoogleLongrunningOperation;

/**
 * The "datasets" collection of methods.
 * Typical usage is:
 *  <code>
 *   $aiplatformService = new Google\Service\Aiplatform(...);
 *   $datasets = $aiplatformService->projects_locations_datasets;
 *  </code>
 */
class ProjectsLocationsDatasets extends \Google\Service\Resource
{
  /**
   * Creates a Dataset. (datasets.create)
   *
   * @param string $parent Required. The resource name of the Location to create
   * the Dataset in. Format: `projects/{project}/locations/{location}`
   * @param GoogleCloudAiplatformV1Dataset $postBody
   * @param array $optParams Optional parameters.
   * @return GoogleLongrunningOperation
   * @throws \Google\Service\Exception
   */
  public function create($parent, GoogleCloudAiplatformV1Dataset $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('create', [$params], GoogleLongrunningOperation::class);
  }
  /**
   * Deletes a Dataset. (datasets.delete)
   *
   * @param string $name Required. The resource name of the Dataset to delete.
   * Format: `projects/{project}/locations/{location}/datasets/{dataset}`
   * @param array $optParams Optional parameters.
   * @return GoogleLongrunningOperation
   * @throws \Google\Service\Exception
   */
  public function delete($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('delete', [$params], GoogleLongrunningOperation::class);
  }
  /**
   * Exports data from a Dataset. (datasets.export)
   *
   * @param string $name Required. The name of the Dataset resource. Format:
   * `projects/{project}/locations/{location}/datasets/{dataset}`
   * @param GoogleCloudAiplatformV1ExportDataRequest $postBody
   * @param array $optParams Optional parameters.
   * @return GoogleLongrunningOperation
   * @throws \Google\Service\Exception
   */
  public function export($name, GoogleCloudAiplatformV1ExportDataRequest $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('export', [$params], GoogleLongrunningOperation::class);
  }
  /**
   * Gets a Dataset. (datasets.get)
   *
   * @param string $name Required. The name of the Dataset resource.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string readMask Mask specifying which fields to read.
   * @return GoogleCloudAiplatformV1Dataset
   * @throws \Google\Service\Exception
   */
  public function get($name, $optParams = [])
  {
    $params = ['name' => $name];
    $params = array_merge($params, $optParams);
    return $this->call('get', [$params], GoogleCloudAiplatformV1Dataset::class);
  }
  /**
   * Imports data into a Dataset. (datasets.import)
   *
   * @param string $name Required. The name of the Dataset resource. Format:
   * `projects/{project}/locations/{location}/datasets/{dataset}`
   * @param GoogleCloudAiplatformV1ImportDataRequest $postBody
   * @param array $optParams Optional parameters.
   * @return GoogleLongrunningOperation
   * @throws \Google\Service\Exception
   */
  public function import($name, GoogleCloudAiplatformV1ImportDataRequest $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('import', [$params], GoogleLongrunningOperation::class);
  }
  /**
   * Lists Datasets in a Location. (datasets.listProjectsLocationsDatasets)
   *
   * @param string $parent Required. The name of the Dataset's parent resource.
   * Format: `projects/{project}/locations/{location}`
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter An expression for filtering the results of the
   * request. For field names both snake_case and camelCase are supported. *
   * `display_name`: supports = and != * `metadata_schema_uri`: supports = and !=
   * * `labels` supports general map functions that is: * `labels.key=value` -
   * key:value equality * `labels.key:* or labels:key - key existence * A key
   * including a space must be quoted. `labels."a key"`. Some examples: *
   * `displayName="myDisplayName"` * `labels.myKey="myValue"`
   * @opt_param string orderBy A comma-separated list of fields to order by,
   * sorted in ascending order. Use "desc" after a field name for descending.
   * Supported fields: * `display_name` * `create_time` * `update_time`
   * @opt_param int pageSize The standard list page size.
   * @opt_param string pageToken The standard list page token.
   * @opt_param string readMask Mask specifying which fields to read.
   * @return GoogleCloudAiplatformV1ListDatasetsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocationsDatasets($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], GoogleCloudAiplatformV1ListDatasetsResponse::class);
  }
  /**
   * Updates a Dataset. (datasets.patch)
   *
   * @param string $name Output only. Identifier. The resource name of the
   * Dataset. Format: `projects/{project}/locations/{location}/datasets/{dataset}`
   * @param GoogleCloudAiplatformV1Dataset $postBody
   * @param array $optParams Optional parameters.
   *
   * @opt_param string updateMask Required. The update mask applies to the
   * resource. For the `FieldMask` definition, see google.protobuf.FieldMask.
   * Updatable fields: * `display_name` * `description` * `labels`
   * @return GoogleCloudAiplatformV1Dataset
   * @throws \Google\Service\Exception
   */
  public function patch($name, GoogleCloudAiplatformV1Dataset $postBody, $optParams = [])
  {
    $params = ['name' => $name, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('patch', [$params], GoogleCloudAiplatformV1Dataset::class);
  }
  /**
   * Searches DataItems in a Dataset. (datasets.searchDataItems)
   *
   * @param string $dataset Required. The resource name of the Dataset from which
   * to search DataItems. Format:
   * `projects/{project}/locations/{location}/datasets/{dataset}`
   * @param array $optParams Optional parameters.
   *
   * @opt_param string annotationFilters An expression that specifies what
   * Annotations will be returned per DataItem. Annotations satisfied either of
   * the conditions will be returned. * `annotation_spec_id` - for = or !=. Must
   * specify `saved_query_id=` - saved query id that annotations should belong to.
   * @opt_param string annotationsFilter An expression for filtering the
   * Annotations that will be returned per DataItem. * `annotation_spec_id` - for
   * = or !=.
   * @opt_param int annotationsLimit If set, only up to this many of Annotations
   * will be returned per DataItemView. The maximum value is 1000. If not set, the
   * maximum value will be used.
   * @opt_param string dataItemFilter An expression for filtering the DataItem
   * that will be returned. * `data_item_id` - for = or !=. * `labeled` - for = or
   * !=. * `has_annotation(ANNOTATION_SPEC_ID)` - true only for DataItem that have
   * at least one annotation with annotation_spec_id = `ANNOTATION_SPEC_ID` in the
   * context of SavedQuery or DataLabelingJob. For example: * `data_item=1` *
   * `has_annotation(5)`
   * @opt_param string dataLabelingJob The resource name of a DataLabelingJob.
   * Format: `projects/{project}/locations/{location}/dataLabelingJobs/{data_label
   * ing_job}` If this field is set, all of the search will be done in the context
   * of this DataLabelingJob.
   * @opt_param string fieldMask Mask specifying which fields of DataItemView to
   * read.
   * @opt_param string orderBy A comma-separated list of fields to order by,
   * sorted in ascending order. Use "desc" after a field name for descending.
   * @opt_param string orderByAnnotation.orderBy A comma-separated list of
   * annotation fields to order by, sorted in ascending order. Use "desc" after a
   * field name for descending. Must also specify saved_query.
   * @opt_param string orderByAnnotation.savedQuery Required. Saved query of the
   * Annotation. Only Annotations belong to this saved query will be considered
   * for ordering.
   * @opt_param string orderByDataItem A comma-separated list of data item fields
   * to order by, sorted in ascending order. Use "desc" after a field name for
   * descending.
   * @opt_param int pageSize Requested page size. Server may return fewer results
   * than requested. Default and maximum page size is 100.
   * @opt_param string pageToken A token identifying a page of results for the
   * server to return Typically obtained via
   * SearchDataItemsResponse.next_page_token of the previous
   * DatasetService.SearchDataItems call.
   * @opt_param string savedQuery The resource name of a SavedQuery(annotation set
   * in UI). Format: `projects/{project}/locations/{location}/datasets/{dataset}/s
   * avedQueries/{saved_query}` All of the search will be done in the context of
   * this SavedQuery.
   * @return GoogleCloudAiplatformV1SearchDataItemsResponse
   * @throws \Google\Service\Exception
   */
  public function searchDataItems($dataset, $optParams = [])
  {
    $params = ['dataset' => $dataset];
    $params = array_merge($params, $optParams);
    return $this->call('searchDataItems', [$params], GoogleCloudAiplatformV1SearchDataItemsResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsDatasets::class, 'Google_Service_Aiplatform_Resource_ProjectsLocationsDatasets');
